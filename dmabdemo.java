import java.math.BigDecimal;

import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.ResultSetMetaData;
import java.sql.Statement;

import java.text.DecimalFormat;
import java.text.MessageFormat;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Enumeration;
import java.util.HashMap;
import java.util.Hashtable;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;

import javax.datamining.ExecutionHandle;
import javax.datamining.ExecutionState;
import javax.datamining.ExecutionStatus;
import javax.datamining.JDMException;
import javax.datamining.MiningAlgorithm;
import javax.datamining.MiningFunction;
import javax.datamining.NamedObject;
import javax.datamining.base.AlgorithmSettings;
import javax.datamining.base.Model;
import javax.datamining.base.Task;
import javax.datamining.data.AttributeDataType;
import javax.datamining.data.ModelSignature;
import javax.datamining.data.PhysicalAttribute;
import javax.datamining.data.PhysicalAttributeFactory;
import javax.datamining.data.PhysicalAttributeRole;
import javax.datamining.data.PhysicalDataSet;
import javax.datamining.data.PhysicalDataSetFactory;
import javax.datamining.data.SignatureAttribute;
import javax.datamining.resource.ConnectionSpec;
import javax.datamining.rule.CompoundPredicate;
import javax.datamining.rule.Rule;
import javax.datamining.rule.SimplePredicate;
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
import javax.datamining.supervised.classification.Lift;
import javax.datamining.supervised.classification.ReceiverOperatingCharacterics;
import javax.datamining.task.BuildTask;
import javax.datamining.task.BuildTaskFactory;
import javax.datamining.task.apply.DataSetApplyTask;
import javax.datamining.task.apply.DataSetApplyTaskFactory;

import oracle.dmt.jdm.algorithm.abn.OraABNModelType;
import oracle.dmt.jdm.algorithm.abn.OraABNSettings;
import oracle.dmt.jdm.algorithm.abn.OraABNSettingsFactory;
import oracle.dmt.jdm.base.OraModel;
import oracle.dmt.jdm.base.OraTask;
import oracle.dmt.jdm.modeldetail.abn.OraABNModelDetail;
import oracle.dmt.jdm.resource.OraConnection;
import oracle.dmt.jdm.resource.OraConnectionFactory;
import oracle.dmt.jdm.rule.OraSimplePredicate;
import oracle.dmt.jdm.supervised.classification.OraLift;
import oracle.dmt.jdm.task.OraBuildTask;
import oracle.dmt.jdm.task.OraTransformationTask;
import oracle.dmt.jdm.task.OraTransformationTaskFactory;
import oracle.dmt.jdm.transform.OraExpressionTransform;
import oracle.dmt.jdm.transform.OraTransformationFactory;
import oracle.dmt.jdm.transform.OraTransformationSequence;
import oracle.dmt.jdm.transform.binning.OraBinningTransform;
import oracle.dmt.jdm.transform.binning.OraCategoricalBinningType;
import oracle.dmt.jdm.transform.binning.OraNumericalBinningType;


// Copyright (c) 2004, 2006, Oracle. All rights reserved.  
// File: dmabdemo.java
//J2SE imports


// Java Data Mining (JDM) standard imports
// Oracle Java Data Mining (JDM) implemented api imports

/**
*   This demo program describes how to use the Oracle Data Mining (ODM) Java API
* to solve a classification problem using Adaptive Bayes Network(ABN) algorithm.
* ------------------------------------------------------------------------------
*                             PROBLEM DEFINITION
* ------------------------------------------------------------------------------
*   How to predict whether a customer responds or not to the new affinity card
* program using a classifier based on ABN algorithm?
* ------------------------------------------------------------------------------
*                             DATA DESCRIPTION
* ------------------------------------------------------------------------------
*   Data for this demo is composed from base tables in the Sales History (SH)
* Schema. The SH schema is an Oracle Database Sample Schema that has the customer
* demographics, purchasing, and response details for the previous affinity card
* programs. Data exploration and preparing the data is a common step before
* doing data mining. Here in this demo, the following views are created in the user
* schema using CUSTOMERS, COUNTRIES and SUPPLIMENTARY_DEMOGRAPHICS tables.
*
* MINING_DATA_BUILD_V:
*   This view collects the previous customers' demographic, purchasing, and affinity
*   card response details for building the model.
*
* MINING_DATA_TEST_V:
*   This view collects the previous customers' demographic, purchasing, and affinity
*   card response details for testing the model.
*
* MINING_DATA_APPLY_V:
*   This view collects the prospective customers' demographic and purchasing
*   details for predicting response to the new affinity card program.
*
* ------------------------------------------------------------------------------
*                             DATA MINING PROCESS
* ------------------------------------------------------------------------------
*   Prepare Data:
*     For ABN, binning transformation is used to reduce the cardinality of
*     higher cardinality attributes. The prepareData() method in this demo program
*     illustrates the preparation of the build data and how we can embed these
*     transformations into the model. Model apply and test operations
*     automatically apply the embedded transformations.
*   Build Model:
*     Mining Model is the prime object in data mining. The buildModel() method
*     illustrates how to build a classification model using the ABN algorithm.
*   Test Model:
*     Classification model performance can be evaluated by computing test
*     metrics like accuracy, confusion matrix, lift and ROC. The testModel() or
*     computeTestMetrics() method illustrates how to perform the test operation to
*     compute various metrics.
*   Apply Model:
*     Predicting the target attribute values is the prime function of
*     classification models. The applyModel() method illustrates how to
*     predict the customer response for affinity card program.
* ------------------------------------------------------------------------------
*                             EXECUTING DEMO PROGRAM
* ------------------------------------------------------------------------------
*   Refer to Oracle Data Mining Administrator's Guide
*   for guidelines to execute this demo program.
*/
public class

dmabdemo
  extends Object
{
  // Connection related data members
  private static javax.datamining.resource.Connection m_dmeConn;
  private static javax.datamining.resource.ConnectionFactory m_dmeConnFactory;
  // Object factories used in this demo program
  private static PhysicalDataSetFactory m_pdsFactory;
  private static PhysicalAttributeFactory m_paFactory;
  private static ClassificationSettingsFactory m_clasFactory;
  private static OraABNSettingsFactory m_abnFactory;
  private static BuildTaskFactory m_buildFactory;
  private static DataSetApplyTaskFactory m_dsApplyFactory;
  private static ClassificationTestTaskFactory m_testFactory;
  private static ClassificationApplySettingsFactory m_applySettingsFactory;
  private static ClassificationTestMetricsTaskFactory m_testMetricsTaskFactory;
  private static OraTransformationFactory m_xformFactory;
  private static OraTransformationTaskFactory m_xformTaskFactory;
  // Global constants
  private static DecimalFormat m_df = new DecimalFormat("##.####");
  private static String TAB = "    ";
  private static String CR = "\n";
  private static String CR_TAB = "\n    ";
  private static String HORZ_LINE = "----";
  private static String UNDERLINE =
    "*************************************";
  private static String RULES_ABN_HEADER =
    "*             Rules                 *";
  // Global data members
  private static OraBinningTransform m_buildDataXform;

  public static void main(String[] args)
  {
    try
    {
      if (args.length != 3)
      {
        System.out.println("Usage: java dmabdemo <Host name>:<Port>:<SID> <User Name> <Password>");
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
      // 4. Prepare data
      prepareData();
      // 5. Build model - Please see dmnbdemo.java to see how to build a model
      //                  with supplied prior probability.
      buildModel();
      // 6. Test model - To compute accuracy and confusion matrix, lift result
      //                 and ROC for the model from an apply output data.
      //                 Please see dnnbdemo.java to see how to test the model
      //                 with a test input data and cost matrix.
      computeTestMetrics();
      // 7. Apply model
      applyModel();
      //8. Start execution of the transformation task that trigger execution of
      //   its dependent task(s), here after completion of the transformation task, 
      //   build task executes; after build task test and apply tasks are executed.
      //   Note that this whole process is executed in the server
      m_dmeConn.execute("abnXFormTask_jdm");
      //9. Monitor the task executions of current and its dependent tasks
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
        // 8. Logout from the Data Mining Engine
        m_dmeConn.close();
      }
      catch (Exception anyExp1)
      {
      } // Ignore
    }
  }

  /**
   * Initialize all object factories used in the demo program.
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
    m_abnFactory =
        (OraABNSettingsFactory) m_dmeConn.getFactory("oracle.dmt.jdm.algorithm.abn.OraABNSettings");
    m_buildFactory =
        (BuildTaskFactory) m_dmeConn.getFactory("javax.datamining.task.BuildTask");
    m_dsApplyFactory =
        (DataSetApplyTaskFactory) m_dmeConn.getFactory("javax.datamining.task.apply.DataSetApplyTask");
    m_testFactory =
        (ClassificationTestTaskFactory) m_dmeConn.getFactory("javax.datamining.supervised.classification.ClassificationTestTask");
    m_applySettingsFactory =
        (ClassificationApplySettingsFactory) m_dmeConn.getFactory("javax.datamining.supervised.classification.ClassificationApplySettings");
    m_testMetricsTaskFactory =
        (ClassificationTestMetricsTaskFactory) m_dmeConn.getFactory("javax.datamining.supervised.classification.ClassificationTestMetricsTask");
    m_xformFactory =
        (OraTransformationFactory) m_dmeConn.getFactory("oracle.dmt.jdm.transform.OraTransformation");
    m_xformTaskFactory =
        (OraTransformationTaskFactory) m_dmeConn.getFactory("oracle.dmt.jdm.task.OraTransformationTask");
  }

  /**
   *   This method illustrates preparation of the data for build, test, and apply
   * operations by using binning transformation.
   *
   *   Here numerical attributes AGE and YRS_RESIDENCE are prepared using
   * quantile binning. Categorical attributes COUNTRY_NAME and OCCUPATION
   * are prepared using top-n binning. For more details about binning,
   * refer to Oracle Data Mining Concepts Guide.
   *
   *   The case id, target attribute, and any attribute with <= 2 distinct values
   * should be excluded from the binning process.  However,
   * we may exclude additional attributes from the binning process to
   * demonstrate how to exclude additional attributes from the binning process.
   *
   *   The following table illustrates customer attributes available for building
   * the mining model and data characteristics and type of binning used
   * for each attribute and number of bins used.
   *
   *   COLUMN_NAME              DATA_TYPE  DISTINCT EXCLUDED  BIN TYPE     BIN #
   *   ------------------       ---------  -------- --------  -----------  -----
   *   CUST_ID                  NUMBER     1500     YES
   *   CUST_GENDER              CHAR       2        YES
   *   AGE                      NUMBER     66       NO        QUANTILE     5
   *   CUST_MARITAL_STATUS      VARCHAR2   7        YES
   *   COUNTRY_NAME             VARCHAR2   19       NO        TOP-N        7
   *   CUST_INCOME_LEVEL        VARCHAR2   12       YES
   *   EDUCATION                VARCHAR2   16       YES
   *   OCCUPATION               VARCHAR2   15       NO        TOP-N        7
   *   HOUSEHOLD_SIZE           VARCHAR2   6        YES
   *   YRS_RESIDENCE            NUMBER     15       NO        QUANTILE     5
   *   AFFINITY_CARD            NUMBER     2        YES
   *   BULK_PACK_DISKETTES      NUMBER     2        YES
   *   FLAT_PANEL_MONITOR       NUMBER     2        YES
   *   HOME_THEATER_PACKAGE     NUMBER     2        YES
   *   BOOKKEEPING_APPLICATION  NUMBER     2        YES
   *   PRINTER_SUPPLIES         NUMBER     1        YES
   *   Y_BOX_GAMES              NUMBER     2        YES
   *   OS_DOC_SET_KANJI         NUMBER     2        YES
   *
   *   Build data binnning transformation produces numerical and categorical
   * bin boundary tables. These tables must be specified for test and apply data
   * to bin the data consistent with the build data.
   *   Following binned tables are created after the execution of this method.
   *
   *   Unprepared Data        --->  Prepared(Binned) Data
   *   ---------------------        ---------------------------
   *   MINING_DATA_BUILD_V          ABN_BINNED_DATA_BUILD_JDM
   *   MINING_DATA_TEST_V           ABN_BINNED_DATA_TEST_JDM
   *   MINING_DATA_APPLY_V          ABN_BINNED_DATA_APPLY_JDM
   */
  public static void prepareData()
    throws JDMException
  {

    boolean isOutputAsView = false;
    String inputDataURI = null;
    String outputDataURI = null;
    String categoricalBinTable = null;
    String numericalBinTable = null;
    OraTransformationTask xformTask = null;
    // 1. Prepare build data
    isOutputAsView = true;
    inputDataURI = "MINING_DATA_BUILD_V";
    outputDataURI = "ABN_PREP_DATA_BUILD_JDM";
    m_buildDataXform = m_xformFactory.createBinningTransform();
    String[] excludeColumnList =
    { "CUST_ID", "CUST_GENDER", "CUST_MARITAL_STATUS", "CUST_INCOME_LEVEL",
      "EDUCATION", "HOUSEHOLD_SIZE", "AFFINITY_CARD",
      "BULK_PACK_DISKETTES", "FLAT_PANEL_MONITOR", "HOME_THEATER_PACKAGE",
      "BOOKKEEPING_APPLICATION", "PRINTER_SUPPLIES", "Y_BOX_GAMES",
      "OS_DOC_SET_KANJI" };
    m_buildDataXform.setExcludeColumnList(excludeColumnList);
    // Categorical binning definition
    m_buildDataXform.setNumberOfBinsForCategorical(7);
    m_buildDataXform.setCategoricalBinningType(OraCategoricalBinningType.top_n);
    // Numerical binning definition
    m_buildDataXform.setNumberOfBinsForNumerical(5);
    m_buildDataXform.setNumericalBinningType(OraNumericalBinningType.quantile);
    //2. Create Transformation sequence
    List xformList =
      new ArrayList(1); //There is only one xform in this example, so capacity specified as 1
    xformList.add(m_buildDataXform);
    OraTransformationSequence xformSeq =
      m_xformFactory.createTransformationSequence(inputDataURI, xformList,
                                                  outputDataURI);
    m_dmeConn.saveObject("abnBuildDataXformSeq_jdm", xformSeq, true);
    //3. Create and execute transformation task
    xformTask =
        m_xformTaskFactory.create("abnBuildDataXformSeq_jdm", false);
    saveTask(xformTask, "abnXFormTask_jdm", null);
  }

  /**
   *   This method illustrates how to build a mining model using the
   * ABN_BINNED_DATA_BUILD_JDM dataset and classification settings
   * with ABN algorithm.
   *
   *   By default, the ABN algorithm uses model type as multi feature. Multi feature
   * doesn't produce model details. Here single feature is used to demonstrate
   * retrieval of model details.
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
    m_dmeConn.saveObject("abnBuildData_jdm", buildData, true);
    // 2. Create & save Classification Settings
    // Create ABN algorithm settings
    OraABNSettings abnAlgo = m_abnFactory.create();
    abnAlgo.setModelType(OraABNModelType.singleFeature);
    // Create ClassificationSettings
    ClassificationSettings buildSettings = m_clasFactory.create();
    buildSettings.setAlgorithmSettings(abnAlgo);
    buildSettings.setTargetAttributeName("AFFINITY_CARD");
    m_dmeConn.saveObject("abnBuildSettings_jdm", buildSettings, true);
    // 3. Create, save & execute Build Task
    BuildTask buildTask = //Build data specification
      //Mining function settings name
      //Mining model name
      m_buildFactory.create("abnBuildData_jdm", "abnBuildSettings_jdm",
                            "abnModel_jdm");
    buildTask.setDescription("abnBuildTask_jdm");
    ((OraBuildTask) buildTask).setTransformationSequenceName("abnBuildDataXformSeq_jdm");
    saveTask(buildTask, "abnBuildTask_jdm", "abnXFormTask_jdm");
  }

  /**
   *   This method illustrates how to compute test metrics using
   * an apply output table that has actual and predicted target values. Here the
   * apply operation is done on ABN_BINNED_DATA_TEST_JDM dataset, which creates
   * an apply output table with actual and predicted target values. Using
   * ClassificationTestMetricsTask test metrics are computed. This produces
   * the same test metrics results as ClassificationTestTask.
   */
  public static void computeTestMetrics()
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
    m_dmeConn.saveObject("abnTestApplyData_jdm", applyData, true);
    // 2. Create & save ClassificationApplySettings
    ClassificationApplySettings clasAS = m_applySettingsFactory.create();
    HashMap srcDestMap = new HashMap();
    srcDestMap.put("AFFINITY_CARD", "AFFINITY_CARD");
    clasAS.setSourceDestinationMap(srcDestMap);
    m_dmeConn.saveObject("abnTestApplySettings_jdm", clasAS, true);
    // 3. Create, store & execute apply Task
    DataSetApplyTask applyTask =
      m_dsApplyFactory.create("abnTestApplyData_jdm", "abnModel_jdm",
                              "abnTestApplySettings_jdm",
                              "ABN_TEST_APPLY_OUTPUT_JDM");
    saveTask(applyTask, "abnTestApplyTask_jdm", "abnBuildTask_jdm");
    // 4. Create & save PhysicalDataSpecification
    PhysicalDataSet applyOutputData =
      m_pdsFactory.create("ABN_TEST_APPLY_OUTPUT_JDM", false);
    applyOutputData.addAttribute(pa);
    m_dmeConn.saveObject("abnTestApplyOutput_jdm", applyOutputData, true);
    // 5. Create a ClassificationTestMetricsTask
    ClassificationTestMetricsTask testMetricsTask =
      // apply output data used as input
      // actual target column
      // predicted target column
      // test metrics result name
      m_testMetricsTaskFactory.create("abnTestApplyOutput_jdm",
                                      "AFFINITY_CARD", "PREDICTION",
                                      "abnComputeTestMetrics_jdm"); // enable confusion matrix computation
    testMetricsTask.computeMetric(ClassificationTestMetricOption.confusionMatrix,
                                  true); // enable lift computation
    testMetricsTask.computeMetric(ClassificationTestMetricOption.lift,
                                  true); // enable ROC computation
    testMetricsTask.computeMetric(ClassificationTestMetricOption.receiverOperatingCharacteristics,
                                  true);
    testMetricsTask.setPositiveTargetValue(new Integer(1));
    testMetricsTask.setNumberOfLiftQuantiles(10);
    testMetricsTask.setPredictionRankingAttrName("PROBABILITY");
    // Save task and specify the parent task name
    saveTask(testMetricsTask, "abnTestMetricsTask_jdm",
             "abnTestApplyTask_jdm");
  }

  /**
   *   This method illustrates how to apply the mining model on the
   * ABN_BINNED_DATA_APPLY_JDM dataset to predict customer
   * response. After completion of the task, the apply output table with the
   * predicted results is created at the user-specified location.
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
    m_dmeConn.saveObject("abnApplyData_jdm", applyData, true);
    // 2. Create & save ClassificationApplySettings
    ClassificationApplySettings clasAS = m_applySettingsFactory.create();
    m_dmeConn.saveObject("abnApplySettings_jdm", clasAS, true);
    // 3. Create, store & execute apply Task
    DataSetApplyTask applyTask =
      m_dsApplyFactory.create("abnApplyData_jdm", "abnModel_jdm",
                              "abnApplySettings_jdm",
                              "ABN_APPLY_OUTPUT_JDM");
    saveTask(applyTask, "abnApplyTask_jdm", "abnBuildTask_jdm");
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

  /**
        * This method monitor task execution initiated by the first task
        * in the sequence of dependent tasks. In addition, this method displays
        * task output results (if any).
        * @throws JDMException
        */
  public static void monitorTaskExecutionProcess()
    throws JDMException
  {
    //Data trasformation task
    //1. Wait for the completion of the task
    System.out.println("---------------------------------------------------");
    System.out.println("--- Prepare Data                                ---");
    System.out.println("---------------------------------------------------");
    System.out.print("Waiting for the completion of abnXFormTask_jdm. ");
    ExecutionStatus xformTaskCompletionStatus =
      m_dmeConn.getLastExecutionHandle("abnXFormTask_jdm").waitForCompletion(Integer.MAX_VALUE);
    //2. If successful
    if (ExecutionState.success.equals(xformTaskCompletionStatus.getState()))
    {
      System.out.println("It is successful. ");
      //BuildTask
      System.out.println("---------------------------------------------------");
      System.out.println("--- Build Model                                 ---");
      System.out.println("---------------------------------------------------");
      //1. Wait for the completion of the task
      System.out.print("Waiting for the completion of abnBuildTask_jdm. ");
      ExecutionStatus buildTaskCompletionStatus =
        m_dmeConn.getLastExecutionHandle("abnBuildTask_jdm").waitForCompletion(Integer.MAX_VALUE);
      //2. If successful
      if (ExecutionState.success.equals(buildTaskCompletionStatus.getState()))
      {
        System.out.println("It is successful. ");
        // 3. Restore the model from the data mining server
        ClassificationModel model =
          (ClassificationModel) m_dmeConn.retrieveObject("abnModel_jdm",
                                                         NamedObject.model);
        // 4. Explore the details of the restored model
        // Display model build settings
        ClassificationSettings retrievedBuildSettings =
          (ClassificationSettings) model.getBuildSettings();
        if (retrievedBuildSettings == null)
          System.out.println("Failure to restore build settings.");
        else
          displayBuildSettings(retrievedBuildSettings,
                               "abnBuildSettings_jdm");
        // Display model details
        OraABNModelDetail abnModelDetails =
          (OraABNModelDetail) model.getModelDetail();
        displayABNModelDetails(abnModelDetails);

        // Display model transformations that are embedded in this example
        OraExpressionTransform exprXforms =
          ((OraModel) model).getModelTransformations();
        displayModelTransforms(exprXforms);

        System.out.println("---------------------------------------------------");
        System.out.println("--- Test Model - using apply output table       ---");
        System.out.println("---------------------------------------------------");

        //If model build is successful, then do testData ApplyTask
        //1. Wait for the completion of the task
        System.out.print("Waiting for the completion of abnTestApplyTask_jdm. ");
        ExecutionStatus testApplyTaskCompletionStatus =
          m_dmeConn.getLastExecutionHandle("abnTestApplyTask_jdm").waitForCompletion(Integer.MAX_VALUE);
        //2. If successful
        if (ExecutionState.success.equals(testApplyTaskCompletionStatus.getState()))
        {
          System.out.println("It is successful. ");
          //If testdata apply is successful, then wait for its child test metrics tasks
          //1. Wait for the completion of the task
          System.out.print("Waiting for the completion of abnTestMetricsTask_jdm. ");
          ExecutionStatus testTaskCompletionStatus =
            m_dmeConn.getLastExecutionHandle("abnTestMetricsTask_jdm").waitForCompletion(Integer.MAX_VALUE);
          //2. If successful
          if (ExecutionState.success.equals(testTaskCompletionStatus.getState()))
          {
            System.out.println("It is successful. ");
            // Restore & display test metrics
            ClassificationTestMetrics testMetrics =
              (ClassificationTestMetrics) m_dmeConn.retrieveObject("abnComputeTestMetrics_jdm",
                                                                   NamedObject.testMetrics);
            // Display classification test metrics
            displayTestMetricDetails(testMetrics);
          }
          else
          {
            System.out.println("It is at state:" +
                               testTaskCompletionStatus.getState().name() +
                               ((testTaskCompletionStatus.getDescription() ==
                                 null)? "":
                                "State Description:" + testTaskCompletionStatus.getDescription()));
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
        //ApplyTask(s)
        //Waiting for apply task to complete
        System.out.println("---------------------------------------------------");
        System.out.println("--- Apply Model                                 ---");
        System.out.println("---------------------------------------------------");

        ExecutionStatus applyTaskStatus =
          m_dmeConn.getLastExecutionHandle("abnApplyTask_jdm").waitForCompletion(Integer.MAX_VALUE);
        if (ExecutionState.success.equals(applyTaskStatus.getState()))
        {
          // Display apply result -- Note that APPLY results do not need to be
          //    reverse transformed, as done in the case of model details. This is
          //    because class values of a classification target were not (required to
          //    be) binned or normalized.
          displayTable("ABN_APPLY_OUTPUT_JDM", "where ROWNUM < 11",
                       "order by CUST_ID");
        }
        else
        {
          System.out.println("abnApplyTask_jdm is at state:" +
                             applyTaskStatus.getState().name() +
                             ((applyTaskStatus.getDescription() == null)?
                              "":
                              "State Description:" + applyTaskStatus.getDescription()));
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
    else
    {
      System.out.println("It is at state:" +
                         xformTaskCompletionStatus.getState().name() +
                         ((xformTaskCompletionStatus.getDescription() ==
                           null)? "":
                          "State Description:" + xformTaskCompletionStatus.getDescription()));
    }
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
      Map map = clasSettings.getPriorProbabilitiesMap(targetAttrName);
      if (map != null)
        System.out.println("Priors Map: " + map.toString());
    }
    catch (Exception jdmExp)
    {
      System.out.println("Failure: clasSettings.getPriorProbabilitiesMap(targetAttrName)throws exception");
      jdmExp.printStackTrace();
    }
  }

  /**
   * This method displayes ABN model signature.
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

  /**
   * This method displayes ABN model details.
   */
  public static void displayABNModelDetails(OraABNModelDetail abnModelDetails)
    throws JDMException
  {
    System.out.println("ABN model details:");
    // Display all model rules
    Collection vRules = abnModelDetails.getRules();
    if (vRules != null && vRules.isEmpty() == false)
    {
      Rule[] rules = (Rule[]) vRules.toArray(new Rule[vRules.size()]);
      System.out.println(CR + CR + UNDERLINE);
      System.out.println(RULES_ABN_HEADER);
      System.out.println(UNDERLINE);

      for (int rl = 0; rl < rules.length; rl++)
      {
        printRuleDetails(rules[rl], 0); // printRuleDetails
      }
    }
    System.out.println(CR + UNDERLINE + CR);
  }

  /**
   * This embedded transformations
   */
  public static void displayModelTransforms(OraExpressionTransform exprXforms)
    throws JDMException
  {
    System.out.println("ABN model transforms:");
    //Get per attribute name and expression element map object
    Map exprMap = exprXforms.getAttributeExpressionMap();
    
    OraExpressionTransform.OraExpressionElement exprElement =
      (OraExpressionTransform.OraExpressionElement) exprMap.get("AGE");
    System.out.println("Attribute Name: AGE");
    System.out.println("Expression: " + exprElement.getExpression());
    System.out.println("Inverse Expression: " + exprElement.getInverseExpression());
    
    exprElement =
       (OraExpressionTransform.OraExpressionElement) exprMap.get("YRS_RESIDENCE");
    System.out.println("Attribute Name: YRS_RESIDENCE");
    System.out.println("Expression: " + exprElement.getExpression());
    System.out.println("Inverse Expression: " + exprElement.getInverseExpression());
  }


  /**
   * Prints rule details
   */
  public static void printRuleDetails(Rule rule, int indent)
  {
    System.out.println(CR_TAB + getIndentation(indent, "Rule Details:") +
                       CR_TAB + TAB +
                       getIndentation(indent, "Support: " + m_df.format(rule.getSupport())) +
                       CR_TAB + TAB +
                       getIndentation(indent, "Confidence: " +
                                      m_df.format(rule.getConfidence())));
    CompoundPredicate antecedent =
      (CompoundPredicate) rule.getAntecedent();
    System.out.println(CR_TAB + TAB +
                       getIndentation(indent, ("Antecedent: ")));
    printAntecedent(antecedent, indent);
    CompoundPredicate consequent =
      (CompoundPredicate) rule.getConsequent();
    System.out.println(CR_TAB + TAB +
                       getIndentation(indent, ("Consequent: ")));
    printConsequent(consequent, indent);
  }

  /**
   * Prints antecedent
   */
  public static void printAntecedent(CompoundPredicate predicate,
                                     int indent)
  {
    try
    {
      SimplePredicate[] sps =
        (SimplePredicate[]) predicate.getPredicates();
      if (sps == null)
        return;
      // Combine predicates by attribute name
      Hashtable htNamePredicateMap = new Hashtable();
      for (int i = 0; i < sps.length; i++)
      {
        StringBuffer simplePredicate = printSimplePredicate(sps[i]);
        String attrName = sps[i].getAttributeName();
        StringBuffer attrTotalPredicate =
          (StringBuffer) htNamePredicateMap.get(attrName);
        if (attrTotalPredicate == null)
          htNamePredicateMap.put(attrName, simplePredicate);
        else
        {
          attrTotalPredicate.append(" AND " + simplePredicate);
        }
      }
      Enumeration en = htNamePredicateMap.keys();
      while (en.hasMoreElements())
      {
        String name = (String) en.nextElement();
        StringBuffer sb = (StringBuffer) htNamePredicateMap.get(name);
        System.out.println(getIndentation(indent,
                                          (TAB + TAB + TAB + sb.toString())));
      }
    }
    catch (Exception e)
    {
      System.out.println("Error printing Antecedant");
    }
  }

  /**
   * Prints consequent
   */
  public static void printConsequent(CompoundPredicate predicate,
                                     int indent)
  {
    try
    {
      SimplePredicate[] sps =
        (SimplePredicate[]) predicate.getPredicates();
      if (sps == null)
        return;
      for (int i = 0; i < sps.length; i++)
      {
        StringBuffer simplePredicate = printSimplePredicate(sps[i]);
        if (i < sps.length - 1)
          simplePredicate.append(" AND ");
        System.out.println(getIndentation(indent,
                                          TAB + TAB + TAB + simplePredicate.toString()));
      }
    }
    catch (Exception e)
    {
      System.out.println("Error printing Consequent");
    }
  }

  /**
   * Prints simple predicate
   */
  public static StringBuffer printSimplePredicate(SimplePredicate predicate)
    throws JDMException
  {
    StringBuffer sb = new StringBuffer();
    if (predicate.isNumericalValue())
    {
      //((OraSimplePredicate)predicate).getComparisonOperatorValue() + " " +
      sb.append(predicate.getAttributeName() + " " +
                ((OraSimplePredicate) predicate).getComparisonOperator().name() +
                " " + predicate.getNumericalValue());
    }
    else
    {
      //((OraSimplePredicate)predicate).getComparisonOperatorValue() + " "
      sb =
          new StringBuffer(predicate.getAttributeName() + " " + ((OraSimplePredicate) predicate).getComparisonOperator().name() +
                           " ");
      Object[] inValues = predicate.getCategoryValues();
      if (inValues != null)
      {
        for (int i = 0; i < inValues.length; i++)
        {
          sb.append(inValues[i]);
          if (i != inValues.length - 1)
            sb.append(",");
        }
      }
    }
    return sb;
  }

  /**
   * Display classification test metrics object
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
    Lift lift = testMetrics.getLift();
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
    ReceiverOperatingCharacterics roc = testMetrics.getROC();
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

  private static String getIndentation(int indent, String sText)
  {
    StringBuffer sbIndent = new StringBuffer(TAB);
    for (int in = 0; in < indent; in++)
      sbIndent.append(TAB);
    StringBuffer outPut = new StringBuffer(sbIndent.toString());
    outPut.append(sText);
    return outPut.toString();
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
      // Build table header
      for (int iCol = 1; iCol <= colCount; iCol++)
      {
        String colName = rsMeta.getColumnName(iCol);
        header.append(emptyCol.replace(0, colName.length(), colName));
        emptyCol = new StringBuffer("                    ");
      }
      System.out.println(header.toString());
      // Write table data
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
    } // Ignore
  }

  private static void clean()
  {
    java.sql.Connection dbConn =
      ((OraConnection) m_dmeConn).getDatabaseConnection();
    Statement stmt = null;
    // Drop the model
    try
    {
      m_dmeConn.removeObject("abnModel_jdm", NamedObject.model);
    }
    catch (JDMException jdmExp)
    {
    }
  }
}
