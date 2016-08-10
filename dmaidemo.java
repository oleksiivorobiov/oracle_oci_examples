// Copyright (c) 2004, 2006, Oracle. All rights reserved.
// File: dmaidemo.java

import java.text.DecimalFormat;
import java.text.MessageFormat;

import java.util.Collection;
import java.util.Iterator;

import javax.datamining.ExecutionHandle;
import javax.datamining.ExecutionState;
import javax.datamining.ExecutionStatus;
import javax.datamining.JDMException;
import javax.datamining.NamedObject;
import javax.datamining.SortOrder;
import javax.datamining.attributeimportance.AttributeImportanceModel;
import javax.datamining.attributeimportance.AttributeImportanceSettings;
import javax.datamining.attributeimportance.AttributeImportanceSettingsFactory;
import javax.datamining.base.Task;
import javax.datamining.data.AttributeDataType;
import javax.datamining.data.ModelSignature;
import javax.datamining.data.PhysicalAttribute;
import javax.datamining.data.PhysicalAttributeFactory;
import javax.datamining.data.PhysicalAttributeRole;
import javax.datamining.data.PhysicalDataSet;
import javax.datamining.data.PhysicalDataSetFactory;
import javax.datamining.resource.Connection;
import javax.datamining.resource.ConnectionFactory;
import javax.datamining.resource.ConnectionSpec;
import javax.datamining.task.BuildTask;
import javax.datamining.task.BuildTaskFactory;

import oracle.dmt.jdm.base.OraBuildSettings;
import oracle.dmt.jdm.resource.OraConnectionFactory;
import oracle.dmt.jdm.task.OraTransformationTaskFactory;
import oracle.dmt.jdm.transform.binning.OraBinningTransformFactory;

/**
*   This demo program describes how to use the Oracle Data Mining (ODM) Java API
* to build an Attribute Importance (AI) model by using the Minimum Description
* Length (MDL) algorithm.
* ------------------------------------------------------------------------------
*                             PROBLEM DEFINITION
* ------------------------------------------------------------------------------
*   Given a target attribute affinity_card, find the importance of independent
* attributes.
* ------------------------------------------------------------------------------
*                             DATA DESCRIPTION
* ------------------------------------------------------------------------------
*  See the corresponding section in dmabdemo.java - Classification using ABN.
* ------------------------------------------------------------------------------
*                             DATA MINING PROCESS
* ------------------------------------------------------------------------------
*
*   Build Model:
*     Mining Model is the prime object in data mining. The buildModel() method
*     illustrates how to build an Attribute Importance model by using Minimum
*     Description Length (MDL) algorithm with auto data preparation.
* ------------------------------------------------------------------------------
*                             EXECUTING DEMO PROGRAM
* ------------------------------------------------------------------------------
*   Refer to Oracle Data Mining Administrator's Guide
*   for guidelines for executing this demo program.
*/
public class dmaidemo
  extends Object
{
  // Connection related data members
  private static Connection m_dmeConn;
  private static ConnectionFactory m_dmeConnFactory;
  // Object factories used in this demo program
  private static PhysicalDataSetFactory m_pdsFactory;
  private static PhysicalAttributeFactory m_paFactory;
  private static AttributeImportanceSettingsFactory m_aiFactory;
  private static BuildTaskFactory m_buildFactory;
  private static OraBinningTransformFactory m_binningXformFactory;
  private static OraTransformationTaskFactory m_xformTaskFactory;
  // Provide schema name that has the data tables/views
  private static String m_buildDataName = "MINING_DATA_BUILD_V";
  private static String m_idColName = "CUST_ID";
  private static String m_pdsName = "aiBuildData_jdm";
  private static String m_settingsName = "aiSettings_jdm";
  private static String m_modelName = "aiModel_jdm";
  private static String m_taskName = "aiBuildTask_jdm";
  private static String m_targetName = "AFFINITY_CARD";
  // Global constants
  private static DecimalFormat m_df = new DecimalFormat("##.####");

  public static void main(String[] args)
  {
    try
    {
      if ((args.length != 0) & (args.length != 3))
      {
        System.out.println("Usage: java dmaidemo ");
        System.out.println("   or: java dmaidemo <Host name>:<Port>:<SID> <User Name> <Password>");
        return;
      }
      String uri = args[0];
      String name = args[1];
      String password = args[2];
      // 1. Login to the Data Mining Engine
      m_dmeConnFactory = new OraConnectionFactory();
      ConnectionSpec connSpec = m_dmeConnFactory.getConnectionSpec();
      connSpec.setURI("jdbc:oracle:thin:@" + uri);
      connSpec.setName(name);
      connSpec.setPassword(password);
      m_dmeConn = m_dmeConnFactory.getConnection(connSpec);
      // 2. Clean up all previuosly created demo objects
      clean();
      // 3. Initialize factories for mining objects
      initFactories();
      // 4. Build a model
      buildModel();
    }
    catch (Exception anyExp)
    {
      anyExp.printStackTrace(System.out);
    }
    finally
    {
      try
      {
        //6. Logout from the Data Mining Engine
        m_dmeConn.close();
      }
      catch (Exception anyExp1)
      {
      } //Ignore
    }
  }

  public static void initFactories()
    throws JDMException
  {
    m_pdsFactory =
        (PhysicalDataSetFactory) m_dmeConn.getFactory("javax.datamining.data.PhysicalDataSet");
    m_paFactory =
        (PhysicalAttributeFactory) m_dmeConn.getFactory("javax.datamining.data.PhysicalAttribute");
    m_aiFactory =
        (AttributeImportanceSettingsFactory) m_dmeConn.getFactory("javax.datamining.attributeimportance.AttributeImportanceSettings");
    m_buildFactory =
        (BuildTaskFactory) m_dmeConn.getFactory("javax.datamining.task.BuildTask");
    m_binningXformFactory =
        (OraBinningTransformFactory) m_dmeConn.getFactory("oracle.dmt.jdm.transform.binning.OraBinningTransform");
    m_xformTaskFactory =
        (OraTransformationTaskFactory) m_dmeConn.getFactory("oracle.dmt.jdm.task.OraTransformationTask");
  }

  /**
   *   This method illustrates how to build attribute importance mining model
   * using MINING_BUILD_DATA_V dataset and mining function settings.
   */
  public static void buildModel()
    throws JDMException
  {
    System.out.println("---------------------------------------------------");
    System.out.println("--- Build Model                                 ---");
    System.out.println("---------------------------------------------------");
    // 1. Create & save PhysicalDataSpecification
    PhysicalDataSet buildData =
      m_pdsFactory.create(m_buildDataName, false);
    PhysicalAttribute pa =
      m_paFactory.create(m_idColName, AttributeDataType.integerType,
                         PhysicalAttributeRole.caseId);
    buildData.addAttribute(pa);
    m_dmeConn.saveObject(m_pdsName, buildData, false);
    // 2. Create & save Mining Function Settings
    // Create AttributeImportanceSettings
    AttributeImportanceSettings buildSettings = m_aiFactory.create();
    buildSettings.setTargetAttributeName(m_targetName);
    ((OraBuildSettings) buildSettings).useAutomatedDataPreparations(true);
    m_dmeConn.saveObject(m_settingsName, buildSettings, true);
    // 3. Create, save & execute Build Task
    BuildTask buildTask = // Build data specification
      // Mining function settings name
      // Mining model name
      m_buildFactory.create(m_pdsName, m_settingsName, m_modelName);
    buildTask.setDescription(m_taskName);
    executeTask(buildTask, m_taskName);
    // 4. Restore the model from the DME and explore the details of the model
    AttributeImportanceModel model =
      (AttributeImportanceModel) m_dmeConn.retrieveObject(m_modelName,
                                                          NamedObject.model);
    // Display model details
    displayAIModelDetails(model);
  }

  /**
   * This method stores the given task with the specified name in the DMS
   * (Data Mining Server),
   * and submits the task for asynchronous execution in the DMS. After
   * completing the task successfully it returns true. If there is a task
   * failure, then it prints the error description and returns false.
   *
   * @param taskObj task object
   * @param taskName name of the task
   *
   * @return boolean returns true when the task is successful
   */
  public static boolean executeTask(Task taskObj, String taskName)
    throws JDMException
  {
    boolean isTaskSuccess = false;
    m_dmeConn.saveObject(taskName, taskObj, true);
    ExecutionHandle execHandle = m_dmeConn.execute(taskName);
    System.out.print(taskName + " is started, please wait. ");
    //Wait for completion of the task
    ExecutionStatus status =
      execHandle.waitForCompletion(Integer.MAX_VALUE);
    //Check the status of the task after completion
    isTaskSuccess = status.getState().equals(ExecutionState.success);
    if (isTaskSuccess)
    {
      //Task completed successfully
      System.out.println(taskName + " is successful.");
    }
    else
    { //Task failed
      System.out.println(taskName + " is failed.\nFailure Description: " +
                         status.getDescription());
    }
    return isTaskSuccess;
  }

  /**
   * This method displayes AI model details.
   * */
  public static void displayAIModelDetails(AttributeImportanceModel model)
    throws JDMException
  {
    String modelName = model.getName();
    System.out.println("Model Name: " + modelName);
    // 1. Get spurious info from the model
    System.out.println("Attribute Count:" + model.getAttributeCount());
    System.out.println("Max Rank:" + model.getMaxRank());
    // 2. List of attribute names ranked by their importance value.
    // NOTE: The attributes that show negative importance values are not
    // important attributes, similar to attributes with importance value of 0.
    // From MDL principles, the negative importance value indicates that the
    // given attribute simply adds to the size of the model - in this case, the
    // list of attributes most relevant to the target - without adding any
    // additional knowledge.
    System.out.println("All attributes in descending order of their importance:");
    Collection attrColl = model.getAttributesByRank(SortOrder.descending);
    displayAttributes(attrColl, model.getSignature());
    // 3. Get attributes by range (first top 5)
    System.out.println("Top 5 attributes:");
    attrColl = model.getAttributesByRank(1, 5);
    displayAttributes(attrColl, model.getSignature());
    // 4. Get attributes by percentage (bottom 20%)
    System.out.println("Top 20% attributes:");
    attrColl = model.getAttributesByPercentage(20.0, SortOrder.descending);
    displayAttributes(attrColl, model.getSignature());
    // 5. Get attributes by percentage (bottom 20%)
    System.out.println("Bottom 20% attributes:");
    attrColl = model.getAttributesByPercentage(20.0, SortOrder.ascending);
    displayAttributes(attrColl, model.getSignature());
  }

  private static void displayAttributes(Collection attrColl,
                                        ModelSignature signature)
  {
    System.out.println("( Attribute Name, Importance, Rank )");
    MessageFormat mfSign = new MessageFormat("( {0}, {1}, {2} )");
    String[] vals = new String[3];
    Iterator attrI = attrColl.iterator();
    while (attrI.hasNext())
    {
      try
      {
        vals[0] = (String) attrI.next();
        vals[1] =
            m_df.format(signature.getAttribute(vals[0]).getImportanceValue()) +
            "";
        vals[2] =
            m_df.format(signature.getAttribute(vals[0]).getRank()) + "";
      }
      catch (Exception e)
      {
      }
      System.out.println(mfSign.format(vals));
    }
  }

  private static void clean()
  {
    // Drop the model
    try
    {
      m_dmeConn.removeObject(m_modelName, NamedObject.model);
    }
    catch (JDMException jdmExp)
    {
    }
  }
}
