// Copyright (c) 2004, 2006, Oracle. All rights reserved.  
// File: dmtreedemo.java

// Generic Java api imports
import java.math.BigDecimal;

import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.ResultSetMetaData;
import java.sql.SQLException;
import java.sql.Statement;

import java.text.DecimalFormat;
import java.text.MessageFormat;

import java.util.Collection;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Stack;
// Java Data Mining (JDM) standard api imports
import java.util.logging.Level;

import javax.datamining.ExecutionHandle;
import javax.datamining.ExecutionState;
import javax.datamining.ExecutionStatus;
import javax.datamining.JDMException;
import javax.datamining.MiningAlgorithm;
import javax.datamining.MiningFunction;
import javax.datamining.NamedObject;
import javax.datamining.SizeUnit;
import javax.datamining.algorithm.tree.TreeHomogeneityMetric;
import javax.datamining.algorithm.tree.TreeSettings;
import javax.datamining.algorithm.tree.TreeSettingsFactory;
import javax.datamining.base.AlgorithmSettings;
import javax.datamining.base.Model;
import javax.datamining.base.Task;
import javax.datamining.data.AttributeDataType;
import javax.datamining.data.CategoryProperty;
import javax.datamining.data.CategorySet;
import javax.datamining.data.CategorySetFactory;
import javax.datamining.data.ModelSignature;
import javax.datamining.data.PhysicalAttribute;
import javax.datamining.data.PhysicalAttributeFactory;
import javax.datamining.data.PhysicalAttributeRole;
import javax.datamining.data.PhysicalDataSet;
import javax.datamining.data.PhysicalDataSetFactory;
import javax.datamining.data.SignatureAttribute;
import javax.datamining.modeldetail.tree.TreeModelDetail;
import javax.datamining.modeldetail.tree.TreeNode;
import javax.datamining.resource.Connection;
import javax.datamining.resource.ConnectionFactory;
import javax.datamining.resource.ConnectionSpec;
import javax.datamining.rule.Predicate;
import javax.datamining.rule.Rule;
import javax.datamining.supervised.classification.ClassificationApplySettings;
import javax.datamining.supervised.classification.ClassificationApplySettingsFactory;
import javax.datamining.supervised.classification.ClassificationModel;
import javax.datamining.supervised.classification.ClassificationSettings;
import javax.datamining.supervised.classification.ClassificationSettingsFactory;
import javax.datamining.supervised.classification.ClassificationTestMetricOption;
import javax.datamining.supervised.classification.ClassificationTestMetrics;
import javax.datamining.supervised.classification.ClassificationTestMetricsTask;
import javax.datamining.supervised.classification.ClassificationTestMetricsTaskFactory;
import javax.datamining.supervised.classification.ClassificationTestTaskFactory;
import javax.datamining.supervised.classification.ConfusionMatrix;
import javax.datamining.supervised.classification.CostMatrix;
import javax.datamining.supervised.classification.CostMatrixFactory;
import javax.datamining.supervised.classification.Lift;
import javax.datamining.supervised.classification.ReceiverOperatingCharacterics;
import javax.datamining.task.BuildTask;
import javax.datamining.task.BuildTaskFactory;
import javax.datamining.task.apply.DataSetApplyTask;
import javax.datamining.task.apply.DataSetApplyTaskFactory;
// Oracle Java Data Mining (JDM) implemented api imports
import oracle.dmt.jdm.algorithm.tree.OraTreeSettings;
import oracle.dmt.jdm.base.OraTask;
import oracle.dmt.jdm.resource.OraConnection;
import oracle.dmt.jdm.resource.OraConnectionFactory;
import oracle.dmt.jdm.modeldetail.tree.OraTreeModelDetail;
import oracle.dmt.jdm.supervised.classification.OraClassificationTestMetrics;
import oracle.dmt.jdm.supervised.classification.OraLift;

/**
*   This demo program describes how to use the Oracle Data Mining (ODM) Java API
* to solve a classification problem using Decision Tree (DT) algorithm.
* ------------------------------------------------------------------------------
*                             PROBLEM DEFINITION
* ------------------------------------------------------------------------------
*   How to predict whether a customer responds or not to the new affinity card
* program using a classifier based on DT algorithm?
* ------------------------------------------------------------------------------
*                             DATA DESCRIPTION
* ------------------------------------------------------------------------------
*   Data for this demo is composed from base tables in the Sales History (SH)
* schema. The SH schema is an Oracle Database Sample Schema that has the customer
* demographics, purchasing, and response details for the previous affinity card
* programs. Data exploration and preparing the data is a common step before
* doing data mining. Here in this demo, the following views are created in the user
* schema using CUSTOMERS, COUNTRIES, and SUPPLIMENTARY_DEMOGRAPHICS tables.
*
* MINING_DATA_BUILD_V:
*   This view collects the previous customers' demographics, purchasing, and affinity
*   card response details for building the model.
*
* MINING_DATA_TEST_V:
*   This view collects the previous customers' demographics, purchasing, and affinity
*   card response details for testing the model.
*
* MINING_DATA_APPLY_V:
*   This view collects the prospective customers' demographics and purchasing
*   details for predicting response for the new affinity card program.
*
* ------------------------------------------------------------------------------
*                             DATA MINING PROCESS
* ------------------------------------------------------------------------------
*   Prepare Data:
*     For decision tree data mining specific data preparations are done
*     implicitely, user needs to do only required data cleansing, derived
*     attributes etc. as part of the ETL.
*
*   Build Model:
*     Mining Model is the prime object in data mining. The buildModel() method
*     illustrates how to build a classification model using DT algorithm.
*
*   Test Model:
*     Classification model performance can be evaluated by computing test
*     metrics like accuracy, confusion matrix, lift and ROC. The testModel() or
*     computeTestMetrics() method illustrates how to perform a test operation to
*     compute various metrics.
*
*   Apply Model:
*     Predicting the target attribute values is the prime function of
*     classification models. The applyModel() method illustrates how to
*     predict the customer response for affinity card program.
* ------------------------------------------------------------------------------
*                             EXECUTING DEMO PROGRAM
* ------------------------------------------------------------------------------
*   Refer to Oracle Data Mining Administrator's Guide
*   for guidelines for executing this demo program.
*/
public class

dmtreedemo
{
  //Connection related data members
  private static Connection m_dmeConn;
  private static ConnectionFactory m_dmeConnFactory;
  //Object factories used in this demo program
  private static PhysicalDataSetFactory m_pdsFactory;
  private static PhysicalAttributeFactory m_paFactory;
  private static ClassificationSettingsFactory m_clasFactory;
  private static TreeSettingsFactory m_treeFactory;
  private static BuildTaskFactory m_buildFactory;
  private static DataSetApplyTaskFactory m_dsApplyFactory;
  private static ClassificationTestTaskFactory m_testFactory;
  private static ClassificationApplySettingsFactory m_applySettingsFactory;
  private static CostMatrixFactory m_costMatrixFactory;
  private static CategorySetFactory m_catSetFactory;
  private static ClassificationTestMetricsTaskFactory m_testMetricsTaskFactory;
  // Global constants
  private static DecimalFormat m_df = new DecimalFormat("##.####");
  private static String m_costMatrixName = null;

  public static void main(String[] args)
  {
    try
    {
      if (args.length != 3)
      {
        System.out.println("Usage: java dmtreedemo <Host name>:<Port>:<SID> <User Name> <Password>");
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
      ((OraConnection) m_dmeConn).getLogger().setLevel(Level.FINEST);
      // 2. Clean up all previuosly created demo objects
      clean();
      // 3. Initialize factories for mining objects
      initFactories();
      m_costMatrixName = createCostMatrix();
      // 4. Create and save task to build the model
      buildModel();
      // 5. Create and save task to test model and add build task
      //    as its parent task.
      computeTestMetrics("DT_TEST_APPLY_OUTPUT_JDM", m_costMatrixName);
      // 6. Create and save task to apply the model
      applyModel();
      //7. Start execution of the build task that trigger execution of
      //   its dependent task(s), here after completion of the build task
      //   test task starts executing and then apply task.
      m_dmeConn.execute("treeBuildTask_jdm");
      //8. Monitor the task executions of current and its dependent tasks
      //   and display task output results
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
        //Ignore
      }
    }
  }

  /**
   * Initialize all object factories used in the demo program.
   *
   * @exception JDMException if factory initalization failed
   */
  public static void initFactories()
    throws JDMException
  {
    m_pdsFactory =
        (PhysicalDataSetFactory) m_dmeConn.getFactory("javax.datamining.data.PhysicalDataSet");
    m_paFactory =
        (PhysicalAttributeFactory) m_dmeConn.getFactory("javax.datamining.data.PhysicalAttribute");
    m_clasFactory =
        (ClassificationSettingsFactory) m_dmeConn.getFactory("javax.datamining.supervised.classification.ClassificationSettings");
    m_treeFactory =
        (TreeSettingsFactory) m_dmeConn.getFactory("javax.datamining.algorithm.tree.TreeSettings");
    m_buildFactory =
        (BuildTaskFactory) m_dmeConn.getFactory("javax.datamining.task.BuildTask");
    m_dsApplyFactory =
        (DataSetApplyTaskFactory) m_dmeConn.getFactory("javax.datamining.task.apply.DataSetApplyTask");
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
    * Create and save cost matrix.
    *
    *   Consider an example where it costs $10 to mail a promotion to a
    * prospective customer and if the prospect becomes a customer, the
    * typical sale including the promotion, is worth $100. Then the cost
    * of missing a customer (i.e. missing a $100 sale) is 10x that of
    * incorrectly indicating that a person is good prospect (spending
    * $10 for the promo). In this case, all prediction errors made by
    * the model are NOT equal. To act on what the model determines to
    * be the most likely (probable) outcome may be a poor choice.
    *
    *   Suppose that the probability of a BUY reponse is 10% for a given
    * prospect. Then the expected revenue from the prospect is:
    *   .10 * $100 - .90 * $10 = $1.
    *
    *   The optimal action, given the cost matrix, is to simply mail the
    * promotion to the customer, because the action is profitable ($1).
    *
    *   In contrast, without the cost matrix, all that can be said is
    * that the most likely response is NO BUY, so don't send the
    * promotion.  This shows that cost matrices can be very important.
    *
    *   The caveat in all this is that the model predicted probabilities
    * may NOT be accurate. For binary targets, a systematic approach to
    * this issue exists. It is ROC, illustrated below.
    *
    *  With ROC computed on a test set, the user can see how various model
    * predicted probability thresholds affect the action of mailing a promotion.
    * Suppose I promote when the probability to BUY exceeds 5, 10, 15%, etc.
    * what return can I expect? Note that the answer to this question does
    * not rely on the predicted probabilities being accurate, only that
    * they are in approximately the correct rank order.
    *
    *   Assuming that the predicted probabilities are accurate, provide the
    * cost matrix table name as input to the RANK_APPLY procedure to get
    * appropriate costed scoring results to determine the most appropriate
    * action.
    *
    *    In this demo, we will create the following cost matrix
    *
    *      ActualTarget  PredictedTarget  Cost
    *      ------------  ---------------  ----
    *      0             0                0
    *      0             1                1
    *      1             0                8
    *      1             1                0
    */
  private static String createCostMatrix()
    throws JDMException
  {
    String costMatrixName = "treeCostMatrix";
    // Create categorySet
    CategorySet catSet =
      m_catSetFactory.create(AttributeDataType.integerType);
    // Add category values
    catSet.addCategory(new Integer(0), CategoryProperty.valid);
    catSet.addCategory(new Integer(1), CategoryProperty.valid);
    // Create cost matrix
    CostMatrix costMatrix = m_costMatrixFactory.create(catSet);
    //                  ActualTarget    PredictedTarget Cost
    //                  ------------    --------------- ----
    costMatrix.setCellValue(new Integer(0), new Integer(0), 0);
    costMatrix.setCellValue(new Integer(0), new Integer(1), 1);
    costMatrix.setCellValue(new Integer(1), new Integer(0), 8);
    costMatrix.setCellValue(new Integer(1), new Integer(1), 0);
    //save cost matrix
    m_dmeConn.saveObject(costMatrixName, costMatrix, true);
    return costMatrixName;
  }

  /**
   *   This method illustrates how to build a mining model using the
   * MINING_DATA_BUILD_V dataset and classification settings with
   * DT algorithm.
   *
   * @exception JDMException if model build failed
   */
  public static void buildModel()
    throws JDMException
  {
    // 1. Create & save PhysicalDataSpecification
    PhysicalDataSet buildData =
      m_pdsFactory.create("MINING_DATA_BUILD_V", false);
    PhysicalAttribute pa =
      m_paFactory.create("CUST_ID", AttributeDataType.integerType,
                         PhysicalAttributeRole.caseId);
    buildData.addAttribute(pa);
    m_dmeConn.saveObject("treeBuildData_jdm", buildData, true);
    //2. Create & save Mining Function Settings
    //
    // Create default tree algorithm settings
    TreeSettings treeAlgo = m_treeFactory.create();
    // Set cost matrix.  A cost matrix is used to influence the weighting of
    // misclassification during model creation (and scoring).
    // See Oracle Data Mining Concepts Guide for more details.
    //
    String costMatrixName = m_costMatrixName;
    //
    // Create ClassificationSettings
    ClassificationSettings buildSettings = m_clasFactory.create();
    buildSettings.setAlgorithmSettings(treeAlgo);
    buildSettings.setCostMatrixName(costMatrixName);
    buildSettings.setTargetAttributeName("AFFINITY_CARD");
    m_dmeConn.saveObject("treeBuildSettings_jdm", buildSettings, true);
    // 3. Create, save & execute Build Task
    BuildTask buildTask =
      m_buildFactory.create("treeBuildData_jdm", "treeBuildSettings_jdm",
                            "treeModel_jdm");
    buildTask.setDescription("treeBuildTask_jdm");
    saveTask(buildTask, "treeBuildTask_jdm", null);
  }


  /**
   *   This method illustrates how to compute test metrics using
   * an apply output table that has actual and predicted target values. Here the
   * apply operation is done on the MINING_DATA_TEST_V dataset. It creates
   * an apply output table with actual and predicted target values. Using
   * ClassificationTestMetricsTask test metrics are computed. This produces
   * the same test metrics results as ClassificationTestTask.
   *
   * @param applyOutputName apply output table name
   * @param costMatrixName table name of the supplied cost matrix
   *
   * @exception JDMException if model test failed
   */
  public static void computeTestMetrics(String applyOutputName,
                                        String costMatrixName)
    throws JDMException
  {
    // 1. Do the apply on test data to create an apply output table
    // Create & save PhysicalDataSpecification
    PhysicalDataSet applyData =
      m_pdsFactory.create("MINING_DATA_TEST_V", false);
    PhysicalAttribute pa =
      m_paFactory.create("CUST_ID", AttributeDataType.integerType,
                         PhysicalAttributeRole.caseId);
    applyData.addAttribute(pa);
    m_dmeConn.saveObject("treeTestApplyData_jdm", applyData, true);
    // 2 Create & save ClassificationApplySettings
    ClassificationApplySettings clasAS = m_applySettingsFactory.create();
    HashMap sourceAttrMap = new HashMap();
    sourceAttrMap.put("AFFINITY_CARD", "AFFINITY_CARD");
    clasAS.setSourceDestinationMap(sourceAttrMap);
    m_dmeConn.saveObject("treeTestApplySettings_jdm", clasAS, true);
    // 3 Create, store & execute apply Task
    DataSetApplyTask applyTask =
      m_dsApplyFactory.create("treeTestApplyData_jdm", "treeModel_jdm",
                              "treeTestApplySettings_jdm",
                              applyOutputName);
    saveTask(applyTask, "treeTestApplyTask_jdm", "treeBuildTask_jdm");

    {
      // Compute test metrics on new created apply output table
      // 4. Create & save PhysicalDataSpecification
      PhysicalDataSet applyOutputData =
        m_pdsFactory.create(applyOutputName, false);
      applyOutputData.addAttribute(pa);
      m_dmeConn.saveObject("treeTestApplyOutput_jdm", applyOutputData,
                           true);
      // 5. Create a ClassificationTestMetricsTask
      ClassificationTestMetricsTask testMetricsTask =
        // apply output data used as input
        // actual target column
        // predicted target column
        // test metrics result name
        m_testMetricsTaskFactory.create("treeTestApplyOutput_jdm",
                                        "AFFINITY_CARD", "PREDICTION",
                                        "dtTestMetrics_jdm"); // enable confusion matrix computation
      testMetricsTask.computeMetric(ClassificationTestMetricOption.confusionMatrix,
                                    true); // enable lift computation
      testMetricsTask.computeMetric(ClassificationTestMetricOption.lift,
                                    true); // enable ROC computation
      testMetricsTask.computeMetric(ClassificationTestMetricOption.receiverOperatingCharacteristics,
                                    true);
      //testMetricsTask.setPositiveTargetValue(new Integer(1));
      testMetricsTask.setNumberOfLiftQuantiles(10);
      testMetricsTask.setPredictionRankingAttrName("PROBABILITY");
      // Store & execute the task
      saveTask(testMetricsTask, "treeTestMetricsTask_jdm",
               "treeTestApplyTask_jdm");
      if (costMatrixName != null)
      {
        ClassificationTestMetricsTask testMetricsCostTask =
          // apply output data used as input
          // actual target column
          // predicted target column
          // test metrics result name
          m_testMetricsTaskFactory.create("treeTestApplyOutput_jdm",
                                          "AFFINITY_CARD", "PREDICTION",
                                          "dtTestMetricsWithCost_jdm"); // enable confusion matrix computation
        testMetricsCostTask.computeMetric(ClassificationTestMetricOption.confusionMatrix,
                                          true); // enable lift computation
        testMetricsCostTask.computeMetric(ClassificationTestMetricOption.lift,
                                          true); // enable ROC computation
        testMetricsCostTask.computeMetric(ClassificationTestMetricOption.receiverOperatingCharacteristics,
                                          true);
        //testMetricsCostTask.setPositiveTargetValue(new Integer(1));
        testMetricsCostTask.setNumberOfLiftQuantiles(10);
        testMetricsCostTask.setPredictionRankingAttrName("PROBABILITY");
        testMetricsCostTask.setCostMatrixName(costMatrixName);
        saveTask(testMetricsCostTask, "treeTMWithCostTask_jdm",
                 "treeTestApplyTask_jdm");
      }
    }
  }


  /**
   *   This method illustrates how to apply the mining model on the
   * MINING_DATA_APPLY_V dataset to predict customer
   * response. After completion of the task apply output table with the
   * predicted results is created at the user specified location.
   *
   * @exception JDMException if model apply failed
   */
  public static void applyModel()
    throws JDMException
  {
    // 1. Create & save PhysicalDataSpecification
    PhysicalDataSet applyData =
      m_pdsFactory.create("MINING_DATA_APPLY_V", false);
    PhysicalAttribute pa =
      m_paFactory.create("CUST_ID", AttributeDataType.integerType,
                         PhysicalAttributeRole.caseId);
    applyData.addAttribute(pa);
    m_dmeConn.saveObject("treeApplyData_jdm", applyData, true);
    // 2. Create & save ClassificationApplySettings
    ClassificationApplySettings clasAS = m_applySettingsFactory.create();
    // Add source attributes
    HashMap sourceAttrMap = new HashMap();
    sourceAttrMap.put("COUNTRY_NAME", "COUNTRY_NAME");
    clasAS.setSourceDestinationMap(sourceAttrMap);
    // Add cost matrix
    clasAS.setCostMatrixName(m_costMatrixName);
    m_dmeConn.saveObject("treeApplySettings_jdm", clasAS, true);
    // 3. Create, store & execute apply Task
    DataSetApplyTask applyTask =
      m_dsApplyFactory.create("treeApplyData_jdm", "treeModel_jdm",
                              "treeApplySettings_jdm",
                              "TREE_APPLY_OUTPUT1_JDM");
    saveTask(applyTask, "treeApplyTask1_jdm", "treeBuildTask_jdm");


    // 1. Create & save PhysicalDataSpecification
    applyData = m_pdsFactory.create("MINING_DATA_APPLY_V", false);
    pa =
        m_paFactory.create("CUST_ID", AttributeDataType.integerType, PhysicalAttributeRole.caseId);
    applyData.addAttribute(pa);
    m_dmeConn.saveObject("treeApplyData_jdm", applyData, true);
    // 2. Create & save ClassificationApplySettings
    clasAS = m_applySettingsFactory.create();
    // Add cost matrix
    clasAS.setCostMatrixName(m_costMatrixName);
    m_dmeConn.saveObject("treeApplySettings_jdm", clasAS, true);
    // 3. Create, store & execute apply Task
    applyTask =
        m_dsApplyFactory.create("treeApplyData_jdm", "treeModel_jdm",
                                "treeApplySettings_jdm",
                                "TREE_APPLY_OUTPUT2_JDM");
    saveTask(applyTask, "treeApplyTask2_jdm", "treeBuildTask_jdm");


    // 1. Create & save PhysicalDataSpecification
    applyData = m_pdsFactory.create("MINING_DATA_APPLY_V", false);
    pa =
        m_paFactory.create("CUST_ID", AttributeDataType.integerType, PhysicalAttributeRole.caseId);
    applyData.addAttribute(pa);
    m_dmeConn.saveObject("treeApplyData_jdm", applyData, true);
    // 2. Create & save ClassificationApplySettings
    clasAS = m_applySettingsFactory.create();
    // Add source attributes
    sourceAttrMap = new HashMap();
    sourceAttrMap.put("AGE", "AGE");
    sourceAttrMap.put("OCCUPATION", "OCCUPATION");
    clasAS.setSourceDestinationMap(sourceAttrMap);
    m_dmeConn.saveObject("treeApplySettings_jdm", clasAS, true);
    // 3. Create, store & execute apply Task
    applyTask =
        m_dsApplyFactory.create("treeApplyData_jdm", "treeModel_jdm",
                                "treeApplySettings_jdm",
                                "TREE_APPLY_OUTPUT3_JDM");
    saveTask(applyTask, "treeApplyTask3_jdm", "treeBuildTask_jdm");

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
    System.out.print("Waiting for the completion of treeBuildTask_jdm. ");
    ExecutionStatus buildTaskCompletionStatus =
      m_dmeConn.getLastExecutionHandle("treeBuildTask_jdm").waitForCompletion(Integer.MAX_VALUE);
    //2. If successful
    if (ExecutionState.success.equals(buildTaskCompletionStatus.getState()))
    {
      System.out.println("It is successful. ");
      displayModelDetails();

      //If model build is successful, then do testData ApplyTask
      //1. Wait for the completion of the task
      System.out.print("Waiting for the completion of treeTestApplyTask_jdm. ");
      ExecutionStatus testApplyTaskCompletionStatus =
        m_dmeConn.getLastExecutionHandle("treeTestApplyTask_jdm").waitForCompletion(Integer.MAX_VALUE);
      //2. If successful
      if (ExecutionState.success.equals(testApplyTaskCompletionStatus.getState()))
      {
        System.out.println("It is successful. ");
        //If testdata apply is successful, then wait for its child test metrics tasks
        //1. Wait for the completion of the task
        System.out.print("Waiting for the completion of treeTestMetricsTask_jdm. ");
        ExecutionStatus testTaskCompletionStatus =
          m_dmeConn.getLastExecutionHandle("treeTestMetricsTask_jdm").waitForCompletion(Integer.MAX_VALUE);
        //2. If successful
        if (ExecutionState.success.equals(testTaskCompletionStatus.getState()))
        {
          System.out.println("It is successful. ");
          displayTestMetrics("dtTestMetrics_jdm", null);
        }
        else
        {
          System.out.println("It is at state:" +
                             testTaskCompletionStatus.getState().name() +
                             ((testTaskCompletionStatus.getDescription() ==
                               null)? "":
                              "State Description:" + testTaskCompletionStatus.getDescription()));
        }

        //TestMetricsTask with cost matrix
        //1. Wait for the completion of the task
        System.out.print("Waiting for the completion of treeTMWithCostTask_jdm. ");
        ExecutionStatus tmCostTaskCompletionStatus =
          m_dmeConn.getLastExecutionHandle("treeTMWithCostTask_jdm").waitForCompletion(Integer.MAX_VALUE);
        //2. If successful
        if (ExecutionState.success.equals(tmCostTaskCompletionStatus.getState()))
        {
          System.out.println("It is successful. ");
          displayTestMetrics("dtTestMetricsWithCost_jdm", m_costMatrixName);
          //If test metrics task is successful, then run the apply tasks
          //ApplyTask(s)
          //Waiting for all apply tasks to complete
          ExecutionStatus applyTask1Status =
            m_dmeConn.getLastExecutionHandle("treeApplyTask1_jdm").waitForCompletion(Integer.MAX_VALUE);
          ExecutionStatus applyTask2Status =
            m_dmeConn.getLastExecutionHandle("treeApplyTask2_jdm").waitForCompletion(Integer.MAX_VALUE);
          ExecutionStatus applyTask3Status =
            m_dmeConn.getLastExecutionHandle("treeApplyTask3_jdm").waitForCompletion(Integer.MAX_VALUE);
          if (ExecutionState.success.equals(applyTask1Status.getState()) &&
              ExecutionState.success.equals(applyTask2Status.getState()) &&
              ExecutionState.success.equals(applyTask3Status.getState()))
          {
            displayApplyResults();
          }
          else
          {
            System.out.println("treeApplyTask1_jdm is at state:" +
                               applyTask1Status.getState().name() +
                               ((applyTask1Status.getDescription() ==
                                 null)? "":
                                "State Description:" + applyTask1Status.getDescription()));
            System.out.println("treeApplyTask2_jdm is at state:" +
                               applyTask2Status.getState().name() +
                               ((applyTask2Status.getDescription() ==
                                 null)? "":
                                "State Description:" + applyTask2Status.getDescription()));
            System.out.println("treeApplyTask3_jdm is at state:" +
                               applyTask3Status.getState().name() +
                               ((applyTask3Status.getDescription() ==
                                 null)? "":
                                "State Description:" + applyTask3Status.getDescription()));
          }
        }
        else
        {
          System.out.println("It is at state:" +
                             tmCostTaskCompletionStatus.getState().name() +
                             ((tmCostTaskCompletionStatus.getDescription() ==
                               null)? "":
                              "State Description:" + tmCostTaskCompletionStatus.getDescription()));
        }


      }
      else
      {
        System.out.println("It is at state:" +
                           testApplyTaskCompletionStatus.getState().name() +
                           ((testApplyTaskCompletionStatus.getDescription() ==
                             null)? "":
                            "State Description:" + testApplyTaskCompletionStatus.getDescription()));
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

  private static void displayBuildSettings(ClassificationSettings clasSettings,
                                           String buildSettingsName)
  {
    System.out.println("BuildSettings Details from the " +
                       buildSettingsName + " table:");
    displayTable(buildSettingsName, "", "order by SETTING_NAME");
    System.out.println("BuildSettings Details from the " +
                       buildSettingsName +
                       " model build settings object:");
    String objName = clasSettings.getName();
    if (objName != null)
      System.out.println("Name = " + objName);
    String objDescription = clasSettings.getDescription();
    if (objDescription != null)
      System.out.println("Description = " + objDescription);
    java.util.Date creationDate = clasSettings.getCreationDate();
    String creator = clasSettings.getCreatorInfo();
    String targetAttrName = clasSettings.getTargetAttributeName();
    System.out.println("Target attribute name = " + targetAttrName);
    AlgorithmSettings algoSettings = clasSettings.getAlgorithmSettings();
    if (algoSettings == null)
      System.out.println("Failure: clasSettings.getAlgorithmSettings() returns null");
    MiningAlgorithm algo = algoSettings.getMiningAlgorithm();
    if (algo == null)
      System.out.println("Failure: algoSettings.getMiningAlgorithm() returns null");
    System.out.println("Algorithm Name: " + algo.name());
    MiningFunction function = clasSettings.getMiningFunction();
    if (function == null)
      System.out.println("Failure: clasSettings.getMiningFunction() returns null");
    System.out.println("Function Name: " + function.name());
    try
    {
      String costMatrixName = clasSettings.getCostMatrixName();
      if (costMatrixName != null)
      {
        System.out.println("Cost Matrix Details from the " +
                           costMatrixName + " table:");
        displayTable(costMatrixName, "",
                     "order by ACTUAL_TARGET_VALUE, PREDICTED_TARGET_VALUE");
      }
    }
    catch (Exception jdmExp)
    {
      System.out.println("Failure: clasSettings.getCostMatrixName()throws exception");
      jdmExp.printStackTrace();
    }
    // List of DT algorithm settings
    // treeAlgo.setBuildHomogeneityMetric(TreeHomogeneityMetric.gini);
    // treeAlgo.setMaxDepth(7);
    // ((OraTreeSettings)treeAlgo).setMinDecreaseInImpurity(0.1, SizeUnit.percentage);
    // treeAlgo.setMinNodeSize( 0.05, SizeUnit.percentage );
    // treeAlgo.setMinNodeSize( 10, SizeUnit.count );
    // ((OraTreeSettings)treeAlgo).setMinDecreaseInImpurity(20, SizeUnit.count);
    TreeHomogeneityMetric homogeneityMetric =
      ((OraTreeSettings) algoSettings).getBuildHomogeneityMetric();
    System.out.println("Homogeneity Metric: " + homogeneityMetric.name());
    int intValue = ((OraTreeSettings) algoSettings).getMaxDepth();
    System.out.println("Max Depth: " + intValue);
    double doubleValue =
      ((OraTreeSettings) algoSettings).getMinNodeSizeForSplit(SizeUnit.percentage);
    System.out.println("MinNodeSizeForSplit (percentage): " +
                       m_df.format(doubleValue));
    doubleValue =
        ((OraTreeSettings) algoSettings).getMinNodeSizeForSplit(SizeUnit.count);
    System.out.println("MinNodeSizeForSplit (count): " +
                       m_df.format(doubleValue));
    /*
    doubleValue = ((OraTreeSettings)algoSettings).getMinNodeSize();
    SizeUnit unit = ((OraTreeSettings)algoSettings).getMinNodeSizeUnit();
    System.out.println("Min Node Size (" + unit.name() +"): " + m_df.format(doubleValue));
    */
    doubleValue =
        ((OraTreeSettings) algoSettings).getMinNodeSize(SizeUnit.count);
    System.out.println("Min Node Size (" + SizeUnit.count.name() + "): " +
                       m_df.format(doubleValue));
    doubleValue =
        ((OraTreeSettings) algoSettings).getMinNodeSize(SizeUnit.percentage);
    System.out.println("Min Node Size (" + SizeUnit.percentage.name() +
                       "): " + m_df.format(doubleValue));
  }

  /**
   * This method displayes DT model signature.
   *
   * @param model model object
   * @exception JDMException if failed to retrieve model signature
   */
  public static void displayModelSignature(Model model)
    throws JDMException
  {
    String modelName = model.getName();
    System.out.println("Model Name: " + modelName);
    ModelSignature modelSignature = model.getSignature();
    System.out.println("ModelSignature Deatils: ( Attribute Name, Attribute Type )");
    MessageFormat mfSign =
      new MessageFormat("                        ( {0}, {1} )");
    String[] vals = new String[3];
    Collection sortedSet = modelSignature.getAttributes();
    Iterator attrIterator = sortedSet.iterator();
    while (attrIterator.hasNext())
    {
      SignatureAttribute attr = (SignatureAttribute) attrIterator.next();
      vals[0] = attr.getName();
      vals[1] = attr.getDataType().name();
      System.out.println(mfSign.format(vals));
    }
  }

  public static void displayModelDetails()
    throws JDMException
  {
    //4. Restore the model from the DME and explore the details of the model
    ClassificationModel model =
      (ClassificationModel) m_dmeConn.retrieveObject("treeModel_jdm",
                                                     NamedObject.model);
    // Display model build settings
    ClassificationSettings retrievedBuildSettings =
      (ClassificationSettings) model.getBuildSettings();
    if (retrievedBuildSettings == null)
      System.out.println("Failure to restore build settings.");
    else
      displayBuildSettings(retrievedBuildSettings,
                           "treeBuildSettings_jdm");
    // Display model signature
    displayModelSignature((Model) model);
    // Display model detail
    TreeModelDetail treeModelDetails =
      (TreeModelDetail) model.getModelDetail();
    displayTreeModelDetailsExtensions(treeModelDetails);
  }


  /**
   * This method displayes DT model details.
   *
   * @param treeModelDetails tree model details object
   * @exception JDMException if failed to retrieve model details
   */
  public static void displayTreeModelDetailsExtensions(TreeModelDetail treeModelDetails)
    throws JDMException
  {
    System.out.println("\nTreeModelDetail: Model name=" + "treeModel_jdm");
    TreeNode root = treeModelDetails.getRootNode();
    System.out.println("\nRoot node: " + root.getIdentifier());

    // get the info for the tree model
    int treeDepth = ((OraTreeModelDetail) treeModelDetails).getTreeDepth();
    System.out.println("Tree depth: " + treeDepth);
    int totalNodes =
      ((OraTreeModelDetail) treeModelDetails).getNumberOfNodes();
    System.out.println("Total number of nodes: " + totalNodes);
    int totalLeaves =
      ((OraTreeModelDetail) treeModelDetails).getNumberOfLeafNodes();
    System.out.println("Total number of leaf nodes: " + totalLeaves);

    Stack nodeStack = new Stack();
    nodeStack.push(root);
    while (!nodeStack.empty())
    {
      TreeNode node = (TreeNode) nodeStack.pop();
      // display this node
      int nodeId = node.getIdentifier();
      long caseCount = node.getCaseCount();
      Object prediction = node.getPrediction();
      int level = node.getLevel();
      int children = node.getNumberOfChildren();
      TreeNode parent = node.getParent();
      System.out.println("\nNode id=" + nodeId + " at level " + level);
      if (parent != null)
        System.out.println("parent: " + parent.getIdentifier() +
                           ", children=" + children);
      System.out.println("Case count: " + caseCount + ", prediction: " +
                         prediction);
      Predicate predicate = node.getPredicate();
      System.out.println("Predicate: " + predicate.toString());
      Predicate[] surrogates = node.getSurrogates();
      if (surrogates != null)
        for (int i = 0; i < surrogates.length; i++)
          System.out.println("Surrogate[" + i + "]: " + surrogates[i]);
      // add child nodes in the stack
      if (children > 0)
      {
        TreeNode[] childNodes = node.getChildren();
        for (int i = 0; i < childNodes.length; i++)
          nodeStack.push(childNodes[i]);
      }
    }

    TreeNode[] allNodes = treeModelDetails.getNodes();
    System.out.print("\nNode identifiers by getNodes():");
    for (int i = 0; i < allNodes.length; i++)
      System.out.print(" " + allNodes[i].getIdentifier());
    System.out.println();

    // display the node identifiers
    int[] nodeIds = treeModelDetails.getNodeIdentifiers();
    System.out.print("Node identifiers by getNodeIdentifiers():");
    for (int i = 0; i < nodeIds.length; i++)
      System.out.print(" " + nodeIds[i]);
    System.out.println();

    TreeNode node = treeModelDetails.getNode(nodeIds.length - 1);
    System.out.println("Node identifier by getNode(" +
                       (nodeIds.length - 1) + "): " +
                       node.getIdentifier());
    Rule rule2 = treeModelDetails.getRule(nodeIds.length - 1);
    System.out.println("Rule identifier by getRule(" +
                       (nodeIds.length - 1) + "): " +
                       rule2.getRuleIdentifier());

    // get the rules and display them
    Collection ruleColl = treeModelDetails.getRules();
    Iterator ruleIterator = ruleColl.iterator();
    while (ruleIterator.hasNext())
    {
      Rule rule = (Rule) ruleIterator.next();
      int ruleId = rule.getRuleIdentifier();
      Predicate antecedent = (Predicate) rule.getAntecedent();
      Predicate consequent = (Predicate) rule.getConsequent();
      System.out.println("\nRULE " + ruleId + ": support=" +
                         rule.getSupport() + " (abs=" +
                         rule.getAbsoluteSupport() + "), confidence=" +
                         rule.getConfidence());
      System.out.println(antecedent);
      System.out.println("=======>");
      System.out.println(consequent);
    }
  }

  public static void displayTestMetrics(String metricsName,
                                        String costMatrixName)
    throws JDMException
  {
    if (costMatrixName != null)
    {
      System.out.println("---------------------------------------------------");
      System.out.println("--- Test Model - using apply output table       ---");
      System.out.println("---            - using cost matrix table        ---");
      System.out.println("---------------------------------------------------");
    }
    else
    {
      System.out.println("---------------------------------------------------");
      System.out.println("--- Test Model - using apply output table       ---");
      System.out.println("---            - using no cost matrix table     ---");
      System.out.println("---------------------------------------------------");
    }
    // Restore & display test metrics
    ClassificationTestMetrics testMetrics =
      (ClassificationTestMetrics) m_dmeConn.retrieveObject(metricsName,
                                                           NamedObject.testMetrics);
    // Display classification test metrics
    displayTestMetricDetails(testMetrics);
  }

  /**
   * Display classification test metrics object
   *
   *  @param testMetrics classification test metrics object
   *  @exception JDMException if failed to retrieve test metric details
   */
  public static void displayTestMetricDetails(ClassificationTestMetrics testMetrics)
    throws JDMException
  {
    // Retrieve Oracle ABN model test metrics deatils extensions
    // Test Metrics Name
    System.out.println("Test Metrics Name = " + testMetrics.getName());
    // Model Name
    System.out.println("Model Name = " + testMetrics.getModelName());
    // Test Data Name
    System.out.println("Test Data Name = " +
                       testMetrics.getTestDataName());
    // Accuracy
    System.out.println("Accuracy = " +
                       m_df.format(testMetrics.getAccuracy().doubleValue()));
    // Confusion Matrix
    ConfusionMatrix confusionMatrix = testMetrics.getConfusionMatrix();
    Collection categories = confusionMatrix.getCategories();
    Iterator xIterator = categories.iterator();
    System.out.println("Confusion Matrix: Accuracy = " +
                       m_df.format(confusionMatrix.getAccuracy()));
    System.out.println("Confusion Matrix: Error = " +
                       m_df.format(confusionMatrix.getError()));
    System.out.println("Confusion Matrix:( Actual, Prection, Value )");
    MessageFormat mf =
      new MessageFormat("                 ( {0}, {1}, {2} )");
    String[] vals = new String[3];
    while (xIterator.hasNext())
    {
      Object actual = xIterator.next();
      vals[0] = actual.toString();
      Iterator yIterator = categories.iterator();
      while (yIterator.hasNext())
      {
        Object predicted = yIterator.next();
        vals[1] = predicted.toString();
        long number =
          confusionMatrix.getNumberOfPredictions(actual, predicted);
        vals[2] = Long.toString(number);
        System.out.println(mf.format(vals));
      }
    }
    // Lift
    Lift lift = ((OraClassificationTestMetrics)testMetrics).getLift("1");
    System.out.println("Lift Details:");
    System.out.println("Lift: Target Attribute Name = " +
                       lift.getTargetAttributeName());
    System.out.println("Lift: Positive Target Value = " +
                       lift.getPositiveTargetValue());
    System.out.println("Lift: Total Cases  = " + lift.getTotalCases());
    System.out.println("Lift: Total Positive Cases = " +
                       lift.getTotalPositiveCases());
    int numberOfQuantiles = lift.getNumberOfQuantiles();
    System.out.println("Lift: Number Of Quantiles = " + numberOfQuantiles);
    System.out.println("Lift: ( QUANTILE_NUMBER, TOTAL_CASES, QUANTILE_PERCENT_SIZE, POSITIVE_CASES, NEGATIVE_CASES, PROBABILITY_THRESHOLD, LIFT, CUMULATIVE_LIFT, GAIN, CUMULATIVE_GAIN, TARGET_DENSITY,  CUMULATIVE_TARGET_DENSITY)");
    MessageFormat mfLift =
     new MessageFormat("      ( {0}, {1}, {2}, {3}, {4}, {5}, {6}, {7}, {8}, {9}, {10} )");    
    double[] totalCasesPerQuantile = ((OraLift)lift).getCases();//Total cases per quantile
    double[] quantilePercentageSize = ((OraLift)lift).getPercentageSize();//Quantile pecentage size
    double[] positiveCasesPerQuantile = ((OraLift)lift).getNumberOfPositiveCases();//Positive target cases per quantile
    double[] negativeCasesPerQuantile = ((OraLift)lift).getNumberOfNegativeCases();//Negative target cases per quantile
    double[] probabilityOrCostThresholdPerQuantile = ((OraLift)lift).getProbabilityOrCostThreshold();//Probability or cost threshold per quantile
    double[] liftPerQuantile = ((OraLift)lift).getLift();//Lift values for all quantiles    
    double[] targetDensityPerQuantile = ((OraLift)lift).getTargetDensity();//Target densities per quantile    
    
    double[] cumulativeLift = ((OraLift)lift).getCumulativeLift();
    double[] cumulativeGain = ((OraLift)lift).getCumulativeGain();
    double[] cumulativeTargetDensity = ((OraLift)lift).getCumulativeTargetDensity();
    
    
    
    String[] messageParams = new String[11];
    
    for (int iQuantile = 0; iQuantile < numberOfQuantiles; iQuantile++)
    {
      //Assign message parameters
      messageParams[0] = Integer.toString(iQuantile+1); //QUANTILE_NUMBER
      messageParams[1] = m_df.format(totalCasesPerQuantile[iQuantile]); //TOTAL_CASES
      messageParams[2] = m_df.format(quantilePercentageSize[iQuantile]);//QUANTILE_PERCENT_SIZE
      messageParams[3] = m_df.format(positiveCasesPerQuantile[iQuantile]); //POSITIVE_CASES
      messageParams[4] = m_df.format(negativeCasesPerQuantile[iQuantile]); //NEGATIVE_CASES
      messageParams[5] = m_df.format(probabilityOrCostThresholdPerQuantile[iQuantile]); //PROBABILITY_THRESHOLD
      messageParams[6] = m_df.format(liftPerQuantile[iQuantile]); //LIFT
      messageParams[7] = m_df.format(cumulativeLift[iQuantile]); //CUMULATIVE_LIFT
      messageParams[8] = m_df.format(cumulativeGain[iQuantile]); //CUMULATIVE_GAIN
      messageParams[9] = m_df.format(targetDensityPerQuantile[iQuantile]); //TARGET_DENSITY
      messageParams[10] = m_df.format(cumulativeTargetDensity[iQuantile]); //CUMULATIVE_TARGET_DENSITY
      System.out.println(mfLift.format(messageParams));
    }
    // ROC
    ReceiverOperatingCharacterics roc = ((OraClassificationTestMetrics)testMetrics).getROC("1");
    System.out.println("ROC Details:");
    System.out.println("ROC: Area Under Curve = " +
                       m_df.format(roc.getAreaUnderCurve()));
    int nROCThresh = roc.getNumberOfThresholdCandidates();
    System.out.println("ROC: Number Of Threshold Candidates = " +
                       nROCThresh);
    System.out.println("ROC: ( INDEX, PROBABILITY, TRUE_POSITIVES, FALSE_NEGATIVES, FALSE_POSITIVES, TRUE_NEGATIVES, TRUE_POSITIVE_FRACTION, FALSE_POSITIVE_FRACTION )");
    MessageFormat mfROC =
      new MessageFormat("     ( {0}, {1}, {2}, {3}, {4}, {5}, {6}, {7} )");
    String[] rocVals = new String[8];
    for (int iROC = 1; iROC <= nROCThresh; iROC++)
    {
      rocVals[0] = Integer.toString(iROC); //INDEX
      rocVals[1] =
          m_df.format(roc.getProbabilityThreshold(iROC)); //PROBABILITY
      rocVals[2] =
          Long.toString(roc.getPositives(iROC, true)); //TRUE_POSITIVES
      rocVals[3] =
          Long.toString(roc.getNegatives(iROC, false)); //FALSE_NEGATIVES
      rocVals[4] =
          Long.toString(roc.getPositives(iROC, false)); //FALSE_POSITIVES
      rocVals[5] =
          Long.toString(roc.getNegatives(iROC, true)); //TRUE_NEGATIVES
      rocVals[6] =
          m_df.format(roc.getHitRate(iROC)); //TRUE_POSITIVE_FRACTION
      rocVals[7] =
          m_df.format(roc.getFalseAlarmRate(iROC)); //FALSE_POSITIVE_FRACTION
      System.out.println(mfROC.format(rocVals));
    }
  }

  public static void displayApplyResults()
  {
    System.out.println("---------------------------------------------------");
    System.out.println("--- Business case 1                             ---");
    System.out.println("--- Find the 10 customers who live in Italy     ---");
    System.out.println("--- that are least expensive to be convinced to ---");
    System.out.println("--- use an affinity card.                       ---");
    System.out.println("---------------------------------------------------");
    // 4. Display apply result -- Note that APPLY results do not need to be
    //    reverse transformed, as done in the case of model details. This is
    //    because class values of a classification target were not (required to
    //    be) binned or normalized.
    //
    // Find the 10 customers who live in Italy that are least expensive to be
    // convinced to use an affinity card.
    displayTable("TREE_APPLY_OUTPUT1_JDM",
                 "where COUNTRY_NAME='Italy' and ROWNUM < 11 ",
                 "order by COST");

    System.out.println("---------------------------------------------------");
    System.out.println("--- Business case 2                             ---");
    System.out.println("--- List ten customers (ordered by their id)    ---");
    System.out.println("--- along with likelihood and cost to use or    ---");
    System.out.println("--- reject the affinity card.                   ---");
    System.out.println("---------------------------------------------------");
    // 4. Display apply result -- Note that APPLY results do not need to be
    //    reverse transformed, as done in the case of model details. This is
    //    because class values of a classification target were not (required to
    //    be) binned or normalized.
    //
    // List ten customers (ordered by their id) along with likelihood and cost
    // to use or reject the affinity card (Note: while this example has a
    // binary target, such a query is useful in multi-class classification -
    // Low, Med, High for example).
    displayTable("TREE_APPLY_OUTPUT2_JDM", "where ROWNUM < 21",
                 "order by CUST_ID, PREDICTION");

    System.out.println("---------------------------------------------------");
    System.out.println("--- Business case 3                             ---");
    System.out.println("--- Find the customers who work in Tech support ---");
    System.out.println("--- and are under 25 who is going to response   ---");
    System.out.println("--- to the new affinity card program.           ---");
    System.out.println("---------------------------------------------------");
    // 4. Display apply result -- Note that APPLY results do not need to be
    //    reverse transformed, as done in the case of model details. This is
    //    because class values of a classification target were not (required to
    //    be) binned or normalized.
    //
    // Find the customers who work in Tech support and are under 25 who is
    // going to response to the new affinity card program.
    displayTable("TREE_APPLY_OUTPUT3_JDM",
                 "where OCCUPATION = 'TechSup' " + "and AGE < 25 " +
                 "and PREDICTION = 1 ", "order by CUST_ID");
  }

  private static void displayTable(String tableName, String whereCause,
                                   String orderByColumn)
  {
    StringBuffer emptyCol = new StringBuffer("                    ");

    java.sql.Connection dbConn =
      ((OraConnection) m_dmeConn).getDatabaseConnection();
    PreparedStatement pStmt = null;
    ResultSet rs = null;
    try
    {
      pStmt =
          dbConn.prepareStatement("SELECT * FROM " + tableName + " " + whereCause +
                                  " " + orderByColumn);
      rs = pStmt.executeQuery();
      ResultSetMetaData rsMeta = rs.getMetaData();
      int colCount = rsMeta.getColumnCount();
      StringBuffer header = new StringBuffer();
      System.out.println("Table : " + tableName);
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

          if (obj instanceof java.lang.Number)
          {
            try
            {
              BigDecimal bd = (BigDecimal) obj;
              if (bd.scale() > 5)
              {
                colContent = m_df.format(obj);
              }
              else
              {
                colContent = bd.toString();
              }
            }
            catch (Exception anyExp)
            {
              colContent = m_df.format(obj);
            }
          }
          else
          {
            if (obj == null)
              colContent = "NULL";
            else
              colContent = obj.toString();
          }

          rowContent.append(" " +
                            emptyCol.replace(0, colContent.length(), colContent));
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
      m_dmeConn.removeObject("treeModel_jdm", NamedObject.model);
    }
    catch (Exception jdmExp)
    {
    }
  }
}
