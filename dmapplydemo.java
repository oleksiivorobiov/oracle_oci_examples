// Copyright (c) 2004, 2006, Oracle. All rights reserved.  
// File: dmapplydemo.java

import java.math.BigDecimal;

import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.ResultSetMetaData;
import java.sql.Statement;

import java.text.DecimalFormat;

import java.util.Collection;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;

import javax.datamining.ExecutionState;
import javax.datamining.ExecutionStatus;
import javax.datamining.JDMException;
import javax.datamining.NamedObject;
import javax.datamining.algorithm.naivebayes.NaiveBayesSettings;
import javax.datamining.algorithm.naivebayes.NaiveBayesSettingsFactory;
import javax.datamining.base.Task;
import javax.datamining.data.AttributeDataType;
import javax.datamining.data.CategoryProperty;
import javax.datamining.data.CategorySet;
import javax.datamining.data.CategorySetFactory;
import javax.datamining.data.ModelSignature;
import javax.datamining.data.PhysicalAttribute;
import javax.datamining.data.PhysicalAttributeFactory;
import javax.datamining.data.PhysicalAttributeRole;
import javax.datamining.data.PhysicalDataRecord;
import javax.datamining.data.PhysicalDataRecordFactory;
import javax.datamining.data.PhysicalDataSet;
import javax.datamining.data.PhysicalDataSetFactory;
import javax.datamining.resource.ConnectionSpec;
import javax.datamining.supervised.classification.ClassificationApplyContent;
import javax.datamining.supervised.classification.ClassificationApplySettings;
import javax.datamining.supervised.classification.ClassificationApplySettingsFactory;
import javax.datamining.supervised.classification.ClassificationModel;
import javax.datamining.supervised.classification.ClassificationSettings;
import javax.datamining.supervised.classification.ClassificationSettingsFactory;
import javax.datamining.supervised.classification.ClassificationTestMetricsTaskFactory;
import javax.datamining.supervised.classification.ClassificationTestTaskFactory;
import javax.datamining.supervised.classification.CostMatrix;
import javax.datamining.supervised.classification.CostMatrixFactory;
import javax.datamining.task.BuildTask;
import javax.datamining.task.BuildTaskFactory;
import javax.datamining.task.apply.DataSetApplyTask;
import javax.datamining.task.apply.DataSetApplyTaskFactory;

import javax.datamining.task.apply.RecordApplyTask;

import javax.datamining.task.apply.RecordApplyTaskFactory;

import oracle.dmt.jdm.base.OraBuildSettings;
import oracle.dmt.jdm.base.OraTask;
import oracle.dmt.jdm.resource.OraConnection;
import oracle.dmt.jdm.resource.OraConnectionFactory;



/**
* This sample program describes how to use the ODM Java API to build, test,
* and apply a classification model using the NaiveBayes(NB) algorithm.
* This program uses the "AP_BINNED_DATA_BUILD_JDM" prepared dataset for building
* a model to predict which customer would respond to a promotion campaign
* ("affinity_card").  It then tests the model by computing model accuracy,
* confusion matrix, and lift results using "AP_BINNED_DATA_TEST_JDM" prepared
* dataset. After testing the model, it applies the built model on the
* "AP_BINNED_DATA_APPLY_JDM" prepared dataset to predict which customers would
* respond to a promotion campaign.
* ------------------------------------------------------------------------------
*                             EXECUTING DEMO PROGRAM
* ------------------------------------------------------------------------------
*   Refer to Oracle Data Mining Administrator's Guide
*   for guidelines for executing this demo program.
*/
public class dmapplydemo
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
  private static DataSetApplyTaskFactory m_dsApplyFactory = null;
  private static ClassificationTestTaskFactory m_testFactory = null;
  private static ClassificationApplySettingsFactory m_applySettingsFactory = null;
  private static CostMatrixFactory m_costMatrixFactory = null;
  private static CategorySetFactory m_catSetFactory = null;
  private static ClassificationTestMetricsTaskFactory m_testMetricsTaskFactory = null;
  private static RecordApplyTaskFactory m_recApplyFactory = null;
  private static PhysicalDataRecordFactory m_pdrFactory = null;
  private static ModelSignature m_modelSignature = null;
  
  public static void main(String[] args)
  {
    try
    {
      if ((args.length != 0) & (args.length != 3))
      {
        System.out.println("Usage: java dmapplydemo ");
        System.out.println("   or: java dmapplydemo <Host name>:<Port>:<SID> <User Name> <Password>");
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
      // 5. Create & save cost matrix
      createCostMatrix();
      // 6. Run apply operations using
      //    various types of apply settings supported
      defaultApply();

      sourceDestinationMapApply();

      mapByRankApply();

      mapByCategoryApply();

      mapTopPredictionApply();

      //7. Start execution of the build task that trigger execution of
      //   its dependent apply tasks.
      m_dmeConn.execute("apBuildTask_jdm");
      //8. Monitor the execution of the tasks in the server
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
    m_pdrFactory = (PhysicalDataRecordFactory)m_dmeConn.getFactory(
          "javax.datamining.data.PhysicalDataRecord");
    m_clasFactory =
        (ClassificationSettingsFactory) m_dmeConn.getFactory("javax.datamining.supervised.classification.ClassificationSettings");
    m_nbFactory =
        (NaiveBayesSettingsFactory) m_dmeConn.getFactory("javax.datamining.algorithm.naivebayes.NaiveBayesSettings");
    m_buildFactory =
        (BuildTaskFactory) m_dmeConn.getFactory("javax.datamining.task.BuildTask");
    m_dsApplyFactory =
        (DataSetApplyTaskFactory) m_dmeConn.getFactory("javax.datamining.task.apply.DataSetApplyTask");
    m_recApplyFactory = (RecordApplyTaskFactory)m_dmeConn.getFactory(
        "javax.datamining.task.apply.RecordApplyTask");
    m_testFactory =
        (ClassificationTestTaskFactory) m_dmeConn.getFactory("javax.datamining.supervised.classification.ClassificationTestTask");
    m_applySettingsFactory =
        (ClassificationApplySettingsFactory) m_dmeConn.getFactory("javax.datamining.supervised.classification.ClassificationApplySettings");
    m_costMatrixFactory =
        (CostMatrixFactory) m_dmeConn.getFactory("javax.datamining.supervised.classification.CostMatrix");
    m_catSetFactory =
        (CategorySetFactory) m_dmeConn.getFactory("javax.datamining.data.CategorySet");
    m_testMetricsTaskFactory =
        (ClassificationTestMetricsTaskFactory) m_dmeConn.getFactory("javax.datamining.supervised.classification.ClassificationTestMetricsTask");        
  }

  /**
   * This method builds a mining model. The build operation requires the training
   * dataset location details and the mining function setting.
   * In this example, we will build a classification model using the prepared
   * build dataset "AP_BINNED_DATA_BUILD_JDM" in the user schema, with the
   * NaiveBayes algorithm. This model can be used to predict which customer
   * would respond to a promotion campaign. After completing the build task, the
   * model named "apModel_jdm" will be created in the DMS.
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
    m_dmeConn.saveObject("apBuildData_jdm", buildData, true);
    //2. Create & save Mining Function Settings
    //Create NB algorithm settings
    NaiveBayesSettings nbAlgo = m_nbFactory.create();
    nbAlgo.setPairwiseThreshold(0.01f);
    nbAlgo.setSingletonThreshold(0.01f);
    //Create ClassificationSettings
    ClassificationSettings buildSettings = m_clasFactory.create();
    buildSettings.setAlgorithmSettings(nbAlgo);
    buildSettings.setTargetAttributeName("affinity_card");
    ((OraBuildSettings) buildSettings).useAutomatedDataPreparations(true);
    //Set target prior probabilities
    Map priorMap = new HashMap();
    priorMap.put(new Double(0), new Double(0.7));
    priorMap.put(new Double(1), new Double(0.3));
    buildSettings.setPriorProbabilitiesMap("affinity_card", priorMap);
    m_dmeConn.saveObject("apBuildSettings_jdm", buildSettings, true);
    //3. Create, save & execute Build Task
    BuildTask buildTask = //Build data specification
      //Mining function settings name
      //Mining model name
      m_buildFactory.create("apBuildData_jdm", "apBuildSettings_jdm",
                            "apModel_jdm");
    buildTask.setDescription("apBuildTask_jdm");
    saveTask(buildTask, "apBuildTask_jdm", null);
  }

  /**
   * This method applies a mining model to a dataset. The apply dataset location
   * details, input model, and the output table location details are required
   * for this operation.
   * In this example, we apply the "apModel_jdm" to the
   * "AP_BINNED_DATA_APPLY_JDM" prepared dataset to predict which customers
   * receive an "affinity_card".  After completing the apply task, an apply
   * output table "ap_def_apply_output_jdm" will be  created in the DMS.
   */
  public static void defaultApply()
    throws JDMException
  {
    //1. Create & save PhysicalDataSpecification
    PhysicalDataSet applyData =
      m_pdsFactory.create("MINING_DATA_APPLY_V", false);
    PhysicalAttribute pa =
      m_paFactory.create("cust_id", AttributeDataType.integerType,
                         PhysicalAttributeRole.caseId);
    applyData.addAttribute(pa);
    m_dmeConn.saveObject("ap_def_ApplyData_jdm", applyData, true);
    //2. Create & save ClassificationApplySettings
    ClassificationApplySettings clasAS = m_applySettingsFactory.create();
    m_dmeConn.saveObject("ap_def_ApplySettings_jdm", clasAS, true);

    //3. Create, store & execute apply Task
    DataSetApplyTask applyTask =
      m_dsApplyFactory.create("ap_def_ApplyData_jdm", "apModel_jdm",
                              "ap_def_ApplySettings_jdm",
                              "ap_def_apply_output_jdm");
    saveTask(applyTask, "ap_def_ApplyTask_jdm", "apBuildTask_jdm");
  }

  /**
   * This method applies a mining model to a dataset. The apply dataset location
   * details, input model and the output table location details are required
   * for this operation.
   * In this example, we apply the "apModel_jdm" to the
   * "AP_BINNED_DATA_APPLY_JDM" prepared dataset to predict which customers
   * receive an "affinity_card".  After completing the apply task, an apply
   * output table "ap_src_apply_output_jdm" will be  created in the DMS.
   */
  public static void sourceDestinationMapApply()
    throws JDMException
  {
    //1. Create & save PhysicalDataSpecification
    PhysicalDataSet applyData =
      m_pdsFactory.create("MINING_DATA_APPLY_V", false);
    PhysicalAttribute pa =
      m_paFactory.create("cust_id", AttributeDataType.integerType,
                         PhysicalAttributeRole.caseId);
    applyData.addAttribute(pa);
    m_dmeConn.saveObject("ap_src_ApplyData_jdm", applyData, true);
    //2. Create & save ClassificationApplySettings
    ClassificationApplySettings clasAS = m_applySettingsFactory.create();
    //2.1 Create a Map of attributes to be included in the output dataset
    Map srcDestMap = new HashMap();
    srcDestMap.put("AGE", "AGE_IN_YRS");
    srcDestMap.put("CUST_INCOME_LEVEL", "CUST_INCOME_LEVEL_IN_$");
    srcDestMap.put("OCCUPATION", "OCCUPATION_AS");
    clasAS.setSourceDestinationMap(srcDestMap);
    m_dmeConn.saveObject("ap_src_ApplySettings_jdm", clasAS, true);
    //3. Create, store & execute apply Task
    DataSetApplyTask applyTask =
      m_dsApplyFactory.create("ap_src_ApplyData_jdm", "apModel_jdm",
                              "ap_src_ApplySettings_jdm",
                              "ap_src_apply_output_jdm");
    saveTask(applyTask, "ap_src_ApplyTask_jdm", "apBuildTask_jdm");
  }

  /**
   * This method applies a mining model to a dataset. The apply dataset location
   * details, input model, and the output table location details are required
   * for this operation.
   * In this example, we apply the "apModel_jdm" to the
   * "AP_BINNED_DATA_APPLY_JDM" prepared dataset to predict which customers
   * receive an "affinity_card".  After completing the apply task, an apply
   * output table "ap_apply_output" will be created in the DMS.
   */
  public static void mapByRankApply()
    throws JDMException
  {
    //1. Create & save PhysicalDataSpecification
    PhysicalDataSet applyData =
      m_pdsFactory.create("MINING_DATA_APPLY_V", false);
    PhysicalAttribute pa =
      m_paFactory.create("cust_id", AttributeDataType.integerType,
                         PhysicalAttributeRole.caseId);
    applyData.addAttribute(pa);
    m_dmeConn.saveObject("ap_rk_ApplyData_jdm", applyData, true);
    //2. Create & save ClassificationApplySettings
    ClassificationApplySettings clasAS = m_applySettingsFactory.create();
    String[] predCategories = new String[]
      { "Top_1_Category" };
    clasAS.mapByRank(ClassificationApplyContent.predictedCategory,
                     predCategories, true);
    m_dmeConn.saveObject("ap_rk_ApplySettings_jdm", clasAS, true);

    //3. Create, store & execute apply Task
    DataSetApplyTask applyTask =
      m_dsApplyFactory.create("ap_rk_ApplyData_jdm", "apModel_jdm",
                              "ap_rk_ApplySettings_jdm",
                              "ap_rk_apply_output_jdm");
    saveTask(applyTask, "ap_rk_ApplyTask_jdm", "apBuildTask_jdm");
  }

  /**
   * This method applies a mining model to a dataset. The apply dataset location
   * details, input model, and the output table location details are required
   * for this operation.
   * In this example, we apply the "apModel_jdm" to the
   * "AP_BINNED_DATA_APPLY_JDM" prepared dataset to predict which customers
   * receive an "affinity_card".  After completing the apply task, an apply
   * output table "ap_cls_apply_output_jdm" will be  created in the DMS.
   */
  public static void mapByCategoryApply()
    throws JDMException
  {
    //1. Create & save PhysicalDataSpecification
    PhysicalDataSet applyData =
      m_pdsFactory.create("MINING_DATA_APPLY_V", false);
    PhysicalAttribute pa =
      m_paFactory.create("cust_id", AttributeDataType.integerType,
                         PhysicalAttributeRole.caseId);
    applyData.addAttribute(pa);
    m_dmeConn.saveObject("ap_cls_ApplyData_jdm", applyData, true);
    //2. Create & save ClassificationApplySettings
    ClassificationApplySettings clasAS = m_applySettingsFactory.create();
    clasAS.mapByCategory(ClassificationApplyContent.probability,
                         new Integer(0), "NotResponds");
    clasAS.mapByCategory(ClassificationApplyContent.probability,
                         new Integer(1), "Responds");
    m_dmeConn.saveObject("ap_cls_ApplySettings_jdm", clasAS, true);
    //3. Create, store & execute apply Task
    DataSetApplyTask applyTask =
      m_dsApplyFactory.create("ap_cls_ApplyData_jdm", "apModel_jdm",
                              "ap_cls_ApplySettings_jdm",
                              "ap_cls_apply_output_jdm");
    saveTask(applyTask, "ap_cls_ApplyTask_jdm", "apBuildTask_jdm");

  }

  /**
   * This method applies a mining model to a dataset. The apply dataset location
   * details, input model, and the output table location details are required
   * for this operation.
   * In this example, we apply the "apModel_jdm" to the
   * "AP_BINNED_DATA_APPLY_JDM" prepared dataset to predict which customers
   * receive an "affinity_card".  After completing the apply task, an apply
   * output table "ap_top_apply_output_jdm" will be  created in the DMS.
   */
  public static void mapTopPredictionApply()
    throws JDMException
  {
    //1. Create & save PhysicalDataSpecification
    PhysicalDataSet applyData =
      m_pdsFactory.create("MINING_DATA_APPLY_V", false);
    PhysicalAttribute pa =
      m_paFactory.create("cust_id", AttributeDataType.integerType,
                         PhysicalAttributeRole.caseId);
    applyData.addAttribute(pa);
    m_dmeConn.saveObject("ap_top_ApplyData_jdm", applyData, true);
    //2. Create & save ClassificationApplySettings
    ClassificationApplySettings clasAS = m_applySettingsFactory.create();
    clasAS.mapTopPrediction(ClassificationApplyContent.predictedCategory,
                            "Responds");
    clasAS.mapTopPrediction(ClassificationApplyContent.probability,
                            "Probability");
    m_dmeConn.saveObject("ap_top_ApplySettings_jdm", clasAS, true);
    //3. Create, store & execute apply Task
    DataSetApplyTask applyTask =
      m_dsApplyFactory.create("ap_top_ApplyData_jdm", "apModel_jdm",
                              "ap_top_ApplySettings_jdm",
                              "ap_top_apply_output_jdm");
    saveTask(applyTask, "ap_top_ApplyTask_jdm", "apBuildTask_jdm");

  }
  
  /**
   * This method applies a mining model to a single record. 
   */
  public static void recordApply(ModelSignature modelSignature) throws JDMException
  {    
    //1. Create a PhysicalDataRecord that contains an model signature attributes      
      PhysicalDataRecord applyInputRecord = m_pdrFactory.create(modelSignature);   
    //2. Specify attribute record values
     applyInputRecord.setValue("AGE", "62");
     applyInputRecord.setValue("BOOKKEEPING_APPLICATION", new Integer(1));
     applyInputRecord.setValue("CUST_GENDER","F");
     applyInputRecord.setValue("CUST_MARITAL_STATUS","Widowed");
     applyInputRecord.setValue("EDUCATION","< Bach.");
     applyInputRecord.setValue("HOME_THEATER_PACKAGE", new Integer(1));
     applyInputRecord.setValue("HOUSEHOLD_SIZE", new Integer(2));
     applyInputRecord.setValue("OCCUPATION","Exec.");
     applyInputRecord.setValue("YRS_RESIDENCE",new Integer(3));
     applyInputRecord.setValue("Y_BOX_GAMES",new Integer(0));
    //3. Create and save ClassificationApplySettings object
     ClassificationApplySettings clasAS = m_applySettingsFactory.create();      
     clasAS.mapTopPrediction(ClassificationApplyContent.predictedCategory, "Responds");
     clasAS.mapTopPrediction(ClassificationApplyContent.probability, "Probability");      
     m_dmeConn.saveObject( "ap_rec_ApplySettings_jdm", clasAS, true);
    //4. Create and execute RecordApplyTask
     RecordApplyTask applyTask = m_recApplyFactory.create(applyInputRecord, 
      "apModel_jdm", "ap_rec_ApplySettings_jdm");
     ExecutionStatus recExecStatus = m_dmeConn.execute(applyTask, null);
     //5. Check the status of the task after completion
     boolean isTaskSuccess = recExecStatus.getState().equals(ExecutionState.success);
     if( isTaskSuccess ) {
      //Task completed successfully
      System.out.println("RecordApplyTask is successful.");
     } else {//Task failed
      System.out.println("RecordApplyTask is failed.\nFailure Description: " + 
          recExecStatus.getDescription() );
     }
     //6. Display prediction for this record
     PhysicalDataRecord applyOutputRecord = applyTask.getOutputRecord();
     Number customerResponds = (Number)applyOutputRecord.getValue("RESPONDS"); 
     String respondsStr = (customerResponds.intValue() == 0)?"do not responds":"responds";
     Number customerRespondsProbability = (Number)applyOutputRecord.getValue("PROBABILITY");
     System.out.println("This customer " + respondsStr + " to the affinity card promotion with probability " + customerRespondsProbability.toString());
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
    ((OraTask) taskObj).overwriteOutput(true); //Since OJDM 11.1
    m_dmeConn.saveObject(taskName, taskObj, true);
  }


  //create and save cost matrix

  private static void createCostMatrix()
    throws JDMException
  {
    //create categorySet
    CategorySet catSet =
      m_catSetFactory.create(AttributeDataType.integerType);
    //Add category values
    catSet.addCategory(new Integer(0), CategoryProperty.valid);
    catSet.addCategory(new Integer(1), CategoryProperty.valid);
    //create cost matrix
    CostMatrix costMatrix = m_costMatrixFactory.create(catSet);
    costMatrix.setCellValue(new Integer(0), new Integer(0), 0);
    costMatrix.setCellValue(new Integer(1), new Integer(1), 0);
    costMatrix.setCellValue(new Integer(0), new Integer(1), 2);
    costMatrix.setCellValue(new Integer(1), new Integer(0), 4);
    //save cost matrix
    m_dmeConn.saveObject("apCostMatrix", costMatrix, true);
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
    //1. Monitor BuildTask
    //Wait for the completion of the task
    System.out.print("Waiting for the completion of apBuildTask_jdm. ");
    ExecutionStatus buildTaskCompletionStatus =
      m_dmeConn.getLastExecutionHandle("apBuildTask_jdm").waitForCompletion(Integer.MAX_VALUE);
    //If successful
    if (ExecutionState.success.equals(buildTaskCompletionStatus.getState()))
    {
      System.out.println("It is successful. ");
      ClassificationModel model = 
        (ClassificationModel)m_dmeConn.retrieveObject(
          "apModel_jdm", NamedObject.model);
      m_modelSignature = model.getSignature();
      recordApply(m_modelSignature);
      //2. Monitor default settings applyTask
      //Wait for the completion of the task
      {
        System.out.print("Waiting for the completion of ap_def_ApplyTask_jdm. ");
        ExecutionStatus defaultApplySettingsTaskStatus =
          m_dmeConn.getLastExecutionHandle("ap_def_ApplyTask_jdm").waitForCompletion(Integer.MAX_VALUE);
        //If successful
        if (ExecutionState.success.equals(defaultApplySettingsTaskStatus.getState()))
        {
          System.out.println("It is successful. ");
          displayApplyResults("ap_def_apply_output_jdm",
                              "cust_id, prediction");
        }
        else
        {
          System.out.println("It is at state:" +
                             defaultApplySettingsTaskStatus.getState().name() +
                             ((defaultApplySettingsTaskStatus.getDescription() ==
                               null)? "":
                              "State Description:" + defaultApplySettingsTaskStatus.getDescription()));
        }
      }

      //3. Monitor source destination settings applyTask
      //Wait for the completion of the task
      {
        System.out.print("Waiting for the completion of ap_src_ApplyTask_jdm. ");
        ExecutionStatus sourceDestinationMappingApplyTaskStatus =
          m_dmeConn.getLastExecutionHandle("ap_src_ApplyTask_jdm").waitForCompletion(Integer.MAX_VALUE);
        //If successful
        if (ExecutionState.success.equals(sourceDestinationMappingApplyTaskStatus.getState()))
        {
          System.out.println("It is successful. ");
          displayApplyResults("ap_src_apply_output_jdm",
                              "cust_id, prediction");
        }
        else
        {
          System.out.println("It is at state:" +
                             sourceDestinationMappingApplyTaskStatus.getState().name() +
                             ((sourceDestinationMappingApplyTaskStatus.getDescription() ==
                               null)? "":
                              "State Description:" + sourceDestinationMappingApplyTaskStatus.getDescription()));
        }
      }

      //4. Monitor rank applyTask
      //Wait for the completion of the task
      {
        System.out.print("Waiting for the completion of ap_rk_ApplyTask_jdm. ");
        ExecutionStatus rankApplyTaskStatus =
          m_dmeConn.getLastExecutionHandle("ap_rk_ApplyTask_jdm").waitForCompletion(Integer.MAX_VALUE);
        //If successful
        if (ExecutionState.success.equals(rankApplyTaskStatus.getState()))
        {
          System.out.println("It is successful. ");
          displayApplyResults("ap_rk_apply_output_jdm",
                              "cust_id, prediction");
        }
        else
        {
          System.out.println("It is at state:" +
                             rankApplyTaskStatus.getState().name() +
                             ((rankApplyTaskStatus.getDescription() ==
                               null)? "":
                              "State Description:" + rankApplyTaskStatus.getDescription()));
        }
      }

      //5. Monitor rank applyTask
      //Wait for the completion of the task
      {
        System.out.print("Waiting for the completion of ap_cls_ApplyTask_jdm. ");
        ExecutionStatus classApplyTaskStatus =
          m_dmeConn.getLastExecutionHandle("ap_cls_ApplyTask_jdm").waitForCompletion(Integer.MAX_VALUE);
        //If successful
        if (ExecutionState.success.equals(classApplyTaskStatus.getState()))
        {
          System.out.println("It is successful. ");
          displayApplyResults("ap_cls_apply_output_jdm",
                              "cust_id, notresponds, responds");
        }
        else
        {
          System.out.println("It is at state:" +
                             classApplyTaskStatus.getState().name() +
                             ((classApplyTaskStatus.getDescription() ==
                               null)? "":
                              "State Description:" + classApplyTaskStatus.getDescription()));
        }
      }

      //6. Monitor top-prediction applyTask
      //Wait for the completion of the task
      {
        System.out.print("Waiting for the completion of ap_top_ApplyTask_jdm. ");
        ExecutionStatus topPredApplyTaskStatus =
          m_dmeConn.getLastExecutionHandle("ap_top_ApplyTask_jdm").waitForCompletion(Integer.MAX_VALUE);
        //If successful
        if (ExecutionState.success.equals(topPredApplyTaskStatus.getState()))
        {
          System.out.println("It is successful. ");
          displayApplyResults("ap_top_apply_output_jdm",
                              "cust_id, prediction");
        }
        else
        {
          System.out.println("It is at state:" +
                             topPredApplyTaskStatus.getState().name() +
                             ((topPredApplyTaskStatus.getDescription() ==
                               null)? "":
                              "State Description:" + topPredApplyTaskStatus.getDescription()));
        }
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

  public static void displayApplyResults(String applyTableName,
                                         String orderBy)
  {
    StringBuffer emptyCol = new StringBuffer("                    ");

    java.sql.Connection dbConn =
      ((OraConnection) m_dmeConn).getDatabaseConnection();
    PreparedStatement pStmt = null;
    ResultSet rs = null;
    try
    {
      pStmt =
          dbConn.prepareStatement("SELECT * FROM " + applyTableName + " WHERE ROWNUM<10 ORDER BY " +
                                  orderBy);
      rs = pStmt.executeQuery();
      ResultSetMetaData rsMeta = rs.getMetaData();
      int colCount = rsMeta.getColumnCount();
      StringBuffer header = new StringBuffer();
      System.out.println("Table : " + applyTableName);
      //Build table header
      for (int iCol = 1; iCol <= colCount; iCol++)
      {
        String colName = rsMeta.getColumnName(iCol);
        header.append(emptyCol.replace(0, colName.length(), colName));
        emptyCol = new StringBuffer("                    ");
      }
      System.out.println(header.toString());
      //Write table data
      while (rs.next())
      {
        StringBuffer rowContent = new StringBuffer();
        for (int iCol = 1; iCol <= colCount; iCol++)
        {
          int sqlType = rsMeta.getColumnType(iCol);
          Object obj = rs.getObject(iCol);
          String colContent = null;
          DecimalFormat df = new DecimalFormat("00000000.##E0");
          if (obj instanceof java.lang.Number)
          {
            try
            {
              BigDecimal bd = (BigDecimal) obj;
              if (bd.scale() > 5)
              {
                colContent = df.format(obj);
              }
              else
              {
                colContent = bd.toString();
              }
            }
            catch (Exception anyExp)
            {
              colContent = df.format(obj);
            }
          }
          else
          {
            colContent = obj.toString();
          }

          rowContent.append(emptyCol.replace(0, colContent.length(),
                                             colContent));
          emptyCol = new StringBuffer("                    ");
        }
        System.out.println(rowContent.toString());
      }
    }
    catch (Exception anySqlExp)
    {
      anySqlExp.printStackTrace();
    } //Ignore
  }

  private static void clean()
  {
    //Drop the model
    try
    {
      m_dmeConn.removeObject("apModel_jdm", NamedObject.model);
    }
    catch (Exception jdmExp)
    {
    }

    
  }
}
