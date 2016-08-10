// Copyright (c) 2001, 2006, Oracle. All rights reserved.
// File: dmexpimpdemo.java

/**
 * This sample program describes how to use the ODM Java API for importing
 * an ODM model to the Data Mining Server(DMS) and exporting an ODM model from
 * the DMS in the native format.
 * To illustrate import/export, this program builds a NaiveBayes model
 * using "IE_BINNED_DATA_BUILD_JDM" prepared dataset. After building the model, it
 * exports the model to "nbexport.dump" file. After exporting the model
 * it imports the exported model.
 *
 * NOTE:
 *   You need to create a writable directory called dm_dump before executing
 *   this demo program.
 *   For example:
 *     CREATE OR REPLACE DIRECTORY dm_dump AS '/user/temp';
 *     GRANT READ, WRITE ON DIRECTORY dm_dump TO <yourname>;
 *------------------------------------------------------------------------------
 *                            EXECUTING DEMO PROGRAM
 *------------------------------------------------------------------------------
 *  Refer to Oracle Data Mining Administrator's Guide
 *  for guidelines for executing this demo program.
 */
// Generic Java Imports
import java.sql.Statement;

import java.util.HashMap;
import java.util.Map;
// Java Data Mining (JDM) standard imports
import javax.datamining.ExecutionHandle;
import javax.datamining.ExecutionState;
import javax.datamining.ExecutionStatus;
import javax.datamining.JDMException;
import javax.datamining.NamedObject;
import javax.datamining.algorithm.naivebayes.NaiveBayesSettings;
import javax.datamining.algorithm.naivebayes.NaiveBayesSettingsFactory;
import javax.datamining.base.Task;
import javax.datamining.data.AttributeDataType;
import javax.datamining.data.PhysicalAttribute;
import javax.datamining.data.PhysicalAttributeFactory;
import javax.datamining.data.PhysicalAttributeRole;
import javax.datamining.data.PhysicalDataSet;
import javax.datamining.data.PhysicalDataSetFactory;
import javax.datamining.resource.ConnectionSpec;
import javax.datamining.supervised.classification.ClassificationSettings;
import javax.datamining.supervised.classification.ClassificationSettingsFactory;
import javax.datamining.task.BuildTask;
import javax.datamining.task.BuildTaskFactory;
import javax.datamining.task.ExportTask;
import javax.datamining.task.ExportTaskFactory;
import javax.datamining.task.ImportTask;
import javax.datamining.task.ImportTaskFactory;
// Oracle Java Data Mining (JDM) implemented api imports
import oracle.dmt.jdm.base.OraBuildSettings;
import oracle.dmt.jdm.base.OraTask;
import oracle.dmt.jdm.resource.OraConnection;
import oracle.dmt.jdm.resource.OraConnectionFactory;
import oracle.dmt.jdm.task.OraTransformationTask;
import oracle.dmt.jdm.task.OraTransformationTaskFactory;
import oracle.dmt.jdm.transform.binning.OraBinningTransformFactory;
import oracle.dmt.jdm.transform.binning.OraBinningTransformImpl;
import oracle.dmt.jdm.transform.binning.OraCategoricalBinningType;
import oracle.dmt.jdm.transform.binning.OraNumericalBinningType;

/**
 * This sample program describes how to use the ODM Java API for importing
 * an ODM model to the Data Mining Server(DMS) and exporting an ODM model from
 * the DMS in the native format.
 * To illustrate import/export, this program builds a NaiveBayes model
 * using "IE_BINNED_DATA_BUILD_JDM" prepared dataset. After building the model, it
 * exports the model to "nbexport.dump" file. After exporting the model
 * it imports the exported model.
 *
 * NOTE:
 *   You need to create a writable directory called dm_dump before executing
 *   this demo program.
 *   For example:
 *     CREATE OR REPLACE DIRECTORY dm_dump AS '/user/temp';
 *     GRANT READ, WRITE ON DIRECTORY dm_dump TO <yourname>;
 *------------------------------------------------------------------------------
 *                            EXECUTING DEMO PROGRAM
 *------------------------------------------------------------------------------
 *  Refer to Oracle Data Mining Administrator's Guide
 *  for guidelines for executing this demo program.
 */
public class

dmexpimpdemo
  extends Object
{

  //Connection related data members
  private static javax.datamining.resource.Connection m_dmeConn = null;
  private static javax.datamining.resource.ConnectionFactory m_dmeConnFactory =
    null;
  //Object factories used in this demo program
  private static PhysicalDataSetFactory m_pdsFactory = null;
  private static PhysicalAttributeFactory m_paFactory = null;
  private static ClassificationSettingsFactory m_clasFactory = null;
  private static NaiveBayesSettingsFactory m_nbFactory = null;
  private static BuildTaskFactory m_buildFactory = null;
  private static ExportTaskFactory m_exportFactory = null;
  private static ImportTaskFactory m_importFactory = null;
  private static OraBinningTransformFactory m_binningXformFactory = null;
  private static OraTransformationTaskFactory m_xformTaskFactory = null;

  public static void main(String[] args)
  {
    try
    {
      if ((args.length != 0) & (args.length != 3))
      {
        System.out.println("Usage: java dmexpimpdemo ");
        System.out.println("   or: java dmexpimpdemo <Host name>:<Port>:<SID> <User Name> <Password>");
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
      // 4. Create and save task to build a naive bayes model
      buildModel();
      // 6. Create and save task to export the built model
      exportModel();
      // 7. Create and save task to import the exported model
      importModel();
      // 8. Execute the model build to start executing the sequenece of tasks
      m_dmeConn.execute("nbExpImpBuildTask_jdm");
      // 9. Monitor process
      monitorTaskExecutionProcess();
    }
    catch (Exception anyExp)
    {
      anyExp.printStackTrace(System.out);
    }
    finally
    {
      try
      {
        // 8. Logout from the Data Mining Engine
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
    m_clasFactory =
        (ClassificationSettingsFactory) m_dmeConn.getFactory("javax.datamining.supervised.classification.ClassificationSettings");
    m_nbFactory =
        (NaiveBayesSettingsFactory) m_dmeConn.getFactory("javax.datamining.algorithm.naivebayes.NaiveBayesSettings");
    m_buildFactory =
        (BuildTaskFactory) m_dmeConn.getFactory("javax.datamining.task.BuildTask");
    m_exportFactory =
        (ExportTaskFactory) m_dmeConn.getFactory("javax.datamining.task.ExportTask");
    m_importFactory =
        (ImportTaskFactory) m_dmeConn.getFactory("javax.datamining.task.ImportTask");
    m_binningXformFactory =
        (OraBinningTransformFactory) m_dmeConn.getFactory("oracle.dmt.jdm.transform.binning.OraBinningTransform");
    m_xformTaskFactory =
        (OraTransformationTaskFactory) m_dmeConn.getFactory("oracle.dmt.jdm.task.OraTransformationTask");
  }

  /**
   * This method builds a mining model. The build operation requires the training
   * dataset location details and the mining function settings.
   * In this example, we will build a classification model using the
   * build dataset "IE_BINNED_DATA_BUILD_JDM" in the user schema, with the
   * NaiveBayes algorithm. This model can be used to predict which customer
   * would respond to a promotion campaign. After completing the build task, the
   * model named "nbModel_jdm" will be created in the DMS.
   */
  public static void buildModel()
    throws JDMException
  {
    //1. Create & save PhysicalDataSpecification
    PhysicalDataSet buildData =
      m_pdsFactory.create("MINING_DATA_BUILD_V", false);
    PhysicalAttribute pa =
      m_paFactory.create("cust_id", AttributeDataType.integerType,
                         PhysicalAttributeRole.caseId);
    buildData.addAttribute(pa);
    m_dmeConn.saveObject("nbExpImpBuildData_jdm", buildData, true);
    //2. Create & save Mining Function Settings
    //Create NB algorithm settings
    NaiveBayesSettings nbAlgo = m_nbFactory.create();
    nbAlgo.setPairwiseThreshold(0.01f);
    nbAlgo.setSingletonThreshold(0.01f);
    //Create ClassificationSettings
    ClassificationSettings buildSettings = m_clasFactory.create();
    buildSettings.setAlgorithmSettings(nbAlgo);
    buildSettings.setTargetAttributeName("affinity_card");
    //Set target prior probabilities
    Map priorMap = new HashMap();
    priorMap.put(new Double(0), new Double(0.7));
    priorMap.put(new Double(1), new Double(0.3));
    buildSettings.setPriorProbabilitiesMap("affinity_card", priorMap);
    //Use auto data prep
    ((OraBuildSettings) buildSettings).useAutomatedDataPreparations(true);
    m_dmeConn.saveObject("nbExpImpBuildSettings_jdm", buildSettings, true);
    //3. Create, save & execute Build Task
    BuildTask buildTask = //Build data specification
      //Mining function settings name
      //Mining model name
      m_buildFactory.create("nbExpImpBuildData_jdm",
                            "nbExpImpBuildSettings_jdm",
                            "nbExpImpModel_jdm");
    buildTask.setDescription("nbExpImpBuildTask_jdm");
    saveTask(buildTask, "nbExpImpBuildTask_jdm", null);
  }

  /**
   * This method exports the mining model into a dump file.
   * In this example, we will export "nbExpImpModel_jdm" and store the exported
   * model into a "nbExport" dump file in "DM_DUMP" directory.
   */
  public static void exportModel()
    throws JDMException
  {
    ExportTask exportTask = m_exportFactory.create();
    exportTask.addObjectName("nbExpImpModel_jdm", NamedObject.model);
    exportTask.setURI("DM_DUMP/nbExport");
    saveTask(exportTask, "nbExportTask_jdm", "nbExpImpBuildTask_jdm");
  }

  /**
   * This method imports the mining model from a dump file.
   * In this example, we will import the model "nbExpImpModel_jdm" from
   * "nbExport" dump file in "DM_DUMP" directory.
   */
  public static void importModel()
    throws JDMException
  {
    //Drop the built model
    try
    {
      m_dmeConn.removeObject("nbExpImpModel_jdm", NamedObject.model);
    }
    catch (Exception jdmExp)
    {
    }

    ImportTask importTask = m_importFactory.create();
    importTask.setURI("DM_DUMP/nbExport%U");
    saveTask(importTask, "nbImportTask_jdm", "nbExportTask_jdm");
  }

  /**
   * Cleanup the previously created objects if any.
   */
  private static void clean()
  {
    //Drop the imported model
    try
    {
      m_dmeConn.removeObject("nbExpImpModel_jdm", NamedObject.model);
    }
    catch (Exception jdmExp)
    {
    }
  }

  /**
     * This method monitor task execution initiated by the first task
     * in the sequence of dependent tasks. In addition, this method displays
     * task output results (if any).
     * @throws JDMException
     */
  public static void monitorTaskExecutionProcess()
    throws JDMException
  {
    //BuildTask
    //1. Wait for the completion of the task
    System.out.print("Waiting for the completion of nbExpImpBuildTask_jdm. ");
    ExecutionStatus buildTaskCompletionStatus =
      m_dmeConn.getLastExecutionHandle("nbExpImpBuildTask_jdm").waitForCompletion(Integer.MAX_VALUE);
    //2. If successful
    if (ExecutionState.success.equals(buildTaskCompletionStatus.getState()))
    {
      System.out.println("It is successful. ");

      //If model build is successful, then do export model task
      //1. Wait for the completion of the task
      System.out.print("Waiting for the completion of nbExportTask_jdm. ");
      ExecutionStatus modelExportTaskCompletionStatus =
        m_dmeConn.getLastExecutionHandle("nbExportTask_jdm").waitForCompletion(Integer.MAX_VALUE);
      //2. If successful
      if (ExecutionState.success.equals(modelExportTaskCompletionStatus.getState()))
      {
        System.out.println("It is successful. ");
        //If model export is successful, then wait for import task completion
        //1. Wait for the completion of the task
        System.out.print("Waiting for the completion of nbImportTask_jdm. ");
        ExecutionStatus modelImportTaskCompletionStatus =
          m_dmeConn.getLastExecutionHandle("nbImportTask_jdm").waitForCompletion(Integer.MAX_VALUE);
        //2. If successful
        if (ExecutionState.success.equals(modelImportTaskCompletionStatus.getState()))
        {
          System.out.println("It is successful. ");
        }
        else
        {
          System.out.println("It is at state:" +
                             modelImportTaskCompletionStatus.getState().name() +
                             ((modelImportTaskCompletionStatus.getDescription() ==
                               null)? "":
                              "State Description:" + modelImportTaskCompletionStatus.getDescription()));
        }
      }
      else
      {
        System.out.println("It is at state:" +
                           modelExportTaskCompletionStatus.getState().name() +
                           ((modelExportTaskCompletionStatus.getDescription() ==
                             null)? "":
                            "State Description:" + modelExportTaskCompletionStatus.getDescription()));
      }
    }
    else
    {
      System.out.println("It is at state:" +
                         buildTaskCompletionStatus.getState().name() +
                         ((buildTaskCompletionStatus.getDescription() ==
                           null)? "":
                          "State Description:" + buildTaskCompletionStatus.getDescription()));
    }
  }

  /**
    * This method saves the given task with the specified task name and task
    * dependency (parent task) in the DME.
    *
    * @param taskObj task object
    * @param taskName name of the task
    * @param parentTaskName name of the parent task
    *
    * @return boolean returns true when the task is successful
    * @exception JDMException if task execution failed
    */
  public static void saveTask(Task taskObj, String taskName,
                              String parentTaskName)
    throws JDMException
  {
    if (parentTaskName != null)
      ((OraTask) taskObj).addDependency(parentTaskName); //Since OJDM 11.1
    //if (!(taskObj instanceof BuildTask))
    ((OraTask) taskObj).overwriteOutput(true); //Since OJDM 11.1
    m_dmeConn.saveObject(taskName, taskObj, true);
  }

  public static void trackTaskExecution(String taskName,
                                        ExecutionHandle execHandle)
    throws JDMException
  {
    System.out.print(taskName + " is started, please wait. ");
    //Wait for completion of the task
    ExecutionStatus status =
      execHandle.waitForCompletion(Integer.MAX_VALUE);
    //Check the status of the task after completion
    boolean isTaskSuccess =
      status.getState().equals(ExecutionState.success);
    if (isTaskSuccess) //Task completed successfully
      System.out.println(taskName + " is successful.");
    else //Task failed
      System.out.println(taskName + " failed.\nFailure Description: " +
                         status.getDescription());
  }

}
