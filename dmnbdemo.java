// Copyright (c) 2004, 2007, Oracle. All rights reserved.  
// File: dmnbdemo.java

/**
*   This demo program describes how to use the Oracle Data Mining (ODM) Java API
* to solve a classification problem using the Naive Bayes(NB) algorithm.
* ------------------------------------------------------------------------------
*                             PROBLEM DEFINITION
* ------------------------------------------------------------------------------
*   How to predict whether a customer responds or not to the new affinity card 
* program using a classifier based on NB algorithm?
* ------------------------------------------------------------------------------
*                             DATA DESCRIPTION
* ------------------------------------------------------------------------------
*   Data for this demo is composed from base tables in the Sales History (SH) 
* Schema. The SH schema is an Oracle Database Sample Schema that has the customer
* demographics, purchasing, and response details for the previous affinity card 
* programs. Data exploration and preparing the data is a common step before 
* doing any data mining. Here in this demo, the following views are created 
* in the user schema using CUSTOMERS, COUNTRIES and SUPPLIMENTARY_DEMOGRAPHICS 
* tables.
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
*   details for predicting response to the new affinity card program.
*
* ------------------------------------------------------------------------------
*                             DATA MINING PROCESS
* ------------------------------------------------------------------------------
*   Prepare Data:
*     For NB, binning transformation is used to reduce the carinality of 
*     higher cardinality attributes. The prepareData() method in this demo program
*     illustrates the preparation of the build, test, and apply data.
*   Build Model:
*     Mining Model is the prime object in data mining. The buildModel() method
*     illustrates how to build a classification model using the NB algorithm.
*   Test Model:
*     Classification model performance can be evaluated by computing test 
*     metrics like accuracy, confusion matrix, lift, and ROC. The testModel() or 
*     computeTestMetrics() method illustrates how to perform the test operation to 
*     compute various metrics.
*   Apply Model:
*     Predicting the target attribute values is the prime function of 
*     classification models. The applyModel() method illustrates how to 
*     predict the customer response for an affinity card program.
* ------------------------------------------------------------------------------
*                             EXECUTING DEMO PROGRAM
* ------------------------------------------------------------------------------
*   Refer to Oracle Data Mining Administrator's Guide 
*   for guidelines for executing this demo program.
*/
// Generic api imports
import java.math.BigDecimal;

import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.ResultSetMetaData;
import java.sql.Statement;

import java.text.DecimalFormat;
import java.text.MessageFormat;

import java.util.Collection;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;

import javax.datamining.ExecutionHandle;
import javax.datamining.ExecutionState;
import javax.datamining.ExecutionStatus;
import javax.datamining.JDMException;
import javax.datamining.MiningAlgorithm;
import javax.datamining.MiningFunction;
import javax.datamining.NamedObject;
import javax.datamining.algorithm.naivebayes.NaiveBayesSettings;
import javax.datamining.algorithm.naivebayes.NaiveBayesSettingsFactory;
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
import javax.datamining.modeldetail.naivebayes.NaiveBayesModelDetail;
import javax.datamining.resource.ConnectionSpec;
import javax.datamining.supervised.classification.ClassificationApplyContent;
import javax.datamining.supervised.classification.ClassificationApplySettings;
import javax.datamining.supervised.classification.ClassificationApplySettingsFactory;
import javax.datamining.supervised.classification.ClassificationModel;
import javax.datamining.supervised.classification.ClassificationSettings;
import javax.datamining.supervised.classification.ClassificationSettingsFactory;
import javax.datamining.supervised.classification.ClassificationTestMetrics;
import javax.datamining.supervised.classification.ClassificationTestMetricsTaskFactory;
import javax.datamining.supervised.classification.ClassificationTestTask;
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

import oracle.dmt.jdm.base.OraBuildSettings;
import oracle.dmt.jdm.base.OraModel;
import oracle.dmt.jdm.modeldetail.naivebayes.OraNaiveBayesModelDetail;
import oracle.dmt.jdm.resource.OraConnection;
import oracle.dmt.jdm.resource.OraConnectionFactory;
import oracle.dmt.jdm.supervised.OraTestTask;
import oracle.dmt.jdm.supervised.classification.OraClassificationApplySettings;
import oracle.dmt.jdm.supervised.classification.OraClassificationModel;
import oracle.dmt.jdm.supervised.classification.OraLift;
import oracle.dmt.jdm.task.OraBuildTask;
import oracle.dmt.jdm.transform.OraExpressionTransform;
import oracle.dmt.jdm.transform.OraTransformationFactory;
import oracle.dmt.jdm.transform.OraTransformationSequence;

/**
*   This demo program describes how to use the Oracle Data Mining (ODM) Java API
* to solve a classification problem using the Naive Bayes(NB) algorithm.
* ------------------------------------------------------------------------------
*                             PROBLEM DEFINITION
* ------------------------------------------------------------------------------
*   How to predict whether a customer responds or not to the new affinity card
* program using a classifier based on NB algorithm?
* ------------------------------------------------------------------------------
*                             DATA DESCRIPTION
* ------------------------------------------------------------------------------
*   Data for this demo is composed from base tables in the Sales History (SH)
* Schema. The SH schema is an Oracle Database Sample Schema that has the customer
* demographics, purchasing, and response details for the previous affinity card
* programs. Data exploration and preparing the data is a common step before
* doing any data mining. Here in this demo, the following views are created
* in the user schema using CUSTOMERS, COUNTRIES and SUPPLIMENTARY_DEMOGRAPHICS
* tables.
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
*   details for predicting response to the new affinity card program.
*
* ------------------------------------------------------------------------------
*                             DATA MINING PROCESS
* ------------------------------------------------------------------------------
*   Prepare Data:
*     For NB, binning transformation is used to reduce the carinality of
*     higher cardinality attributes. The prepareData() method in this demo program
*     illustrates the preparation of the build, test, and apply data.
*   Build Model:
*     Mining Model is the prime object in data mining. The buildModel() method
*     illustrates how to build a classification model using the NB algorithm.
*   Test Model:
*     Classification model performance can be evaluated by computing test
*     metrics like accuracy, confusion matrix, lift, and ROC. The testModel() or
*     computeTestMetrics() method illustrates how to perform the test operation to
*     compute various metrics.
*   Apply Model:
*     Predicting the target attribute values is the prime function of
*     classification models. The applyModel() method illustrates how to
*     predict the customer response for an affinity card program.
* ------------------------------------------------------------------------------
*                             EXECUTING DEMO PROGRAM
* ------------------------------------------------------------------------------
*   Refer to Oracle Data Mining Administrator's Guide
*   for guidelines for executing this demo program.
*/
// Java Data Mining (JDM) standard imports
// Oracle Java Data Mining (JDM) implemented api imports

public class dmnbdemo extends Object {
  // Connection related data members
  private static javax.datamining.resource.Connection m_dmeConn;
  private static javax.datamining.resource.ConnectionFactory m_dmeConnFactory;
  // Object factories used in this demo program
  private static PhysicalDataSetFactory m_pdsFactory;
  private static PhysicalAttributeFactory m_paFactory;
  private static ClassificationSettingsFactory m_clasFactory;
  private static NaiveBayesSettingsFactory m_nbFactory;
  private static BuildTaskFactory m_buildFactory;
  private static DataSetApplyTaskFactory m_dsApplyFactory;
  private static ClassificationTestTaskFactory m_testFactory;
  private static ClassificationApplySettingsFactory m_applySettingsFactory;
  private static CostMatrixFactory m_costMatrixFactory;
  private static CategorySetFactory m_catSetFactory;
  private static ClassificationTestMetricsTaskFactory m_testMetricsTaskFactory;
  private static OraTransformationFactory m_xformFactory;
  // Global constants 
  private static DecimalFormat m_df = new DecimalFormat("##.####");
  
  public static void main( String args[] ) { 
    try {
        if (( args.length != 0 ) & ( args.length != 3 )) {
          System.out.println("Usage: java dmnbdemo ");
          System.out.println("   or: java dmnbdemo <Host name>:<Port>:<SID> <User Name> <Password>");
          return;
        }
        String uri = args[0];
        String name =  args[1];
        String password = args[2]; 
        // 1. Login to the Data Mining Engine
        m_dmeConnFactory = new OraConnectionFactory();
        ConnectionSpec connSpec = m_dmeConnFactory.getConnectionSpec();        
        connSpec.setURI("jdbc:oracle:thin:@"+uri); 
        connSpec.setName(name); 
        connSpec.setPassword(password);
        m_dmeConn = m_dmeConnFactory.getConnection(connSpec); 
        // 2. Clean up all previuosly created demo objects
        clean();
        // 3. Initialize factories for mining objects
        initFactories();
        // 4. Build model with supplied prior probability
        buildModel();
        // 5. Test model - To compute accuracy and confusion matrix, lift result
        //                 and ROC for the model from a test input data.
        String costMatrixName = createCostMatrix();
        //Add cost matrix to the model
        OraClassificationModel.addCostMatrix(m_dmeConn, "nbModel_jdm", costMatrixName);
        // Test the model with cost matrix 
        boolean useCostMatrix = true;         
        testModel("nbTestMetricsWithCost_jdm", useCostMatrix);
        // Test the model without cost matrix
        useCostMatrix = false;
        testModel("nbTestMetrics_jdm", useCostMatrix);        
        //Remove cost matrix from the model after testing is done
         OraClassificationModel.removeCostMatrix(m_dmeConn, "nbModel_jdm");
        // 6. Apply model
        applyModel();
        // 7. Rank Apply model with cost matrix 
        rankApplyModel(costMatrixName);
    } catch(Exception anyExp) {
      anyExp.printStackTrace(System.out);
    } finally {
      try {
        // 8. Logout from the Data Mining Engine
        m_dmeConn.close();
      } catch(Exception anyExp1) { } // Ignore
    }
  }

  /**
   * Initialize all object factories used in the demo program.
   * 
   * @exception JDMException if factory initalization failed
   */
  public static void initFactories() throws JDMException
  {
    m_pdsFactory = (PhysicalDataSetFactory)m_dmeConn.getFactory(
      "javax.datamining.data.PhysicalDataSet");
    m_paFactory = (PhysicalAttributeFactory)m_dmeConn.getFactory(
      "javax.datamining.data.PhysicalAttribute");
    m_clasFactory = (ClassificationSettingsFactory)m_dmeConn.getFactory(
      "javax.datamining.supervised.classification.ClassificationSettings");
    m_nbFactory = (NaiveBayesSettingsFactory)m_dmeConn.getFactory(
      "javax.datamining.algorithm.naivebayes.NaiveBayesSettings");
    m_buildFactory = (BuildTaskFactory)m_dmeConn.getFactory(
      "javax.datamining.task.BuildTask");
    m_dsApplyFactory = (DataSetApplyTaskFactory)m_dmeConn.getFactory(
      "javax.datamining.task.apply.DataSetApplyTask");
    m_testFactory = (ClassificationTestTaskFactory)m_dmeConn.getFactory(
      "javax.datamining.supervised.classification.ClassificationTestTask");
    m_applySettingsFactory = (ClassificationApplySettingsFactory)m_dmeConn.getFactory(
      "javax.datamining.supervised.classification.ClassificationApplySettings");
    m_costMatrixFactory = (CostMatrixFactory)m_dmeConn.getFactory(
      "javax.datamining.supervised.classification.CostMatrix");
    m_catSetFactory = (CategorySetFactory)m_dmeConn.getFactory(
      "javax.datamining.data.CategorySet" );
    m_testMetricsTaskFactory = (ClassificationTestMetricsTaskFactory)m_dmeConn.getFactory(
      "javax.datamining.supervised.classification.ClassificationTestMetricsTask");
    m_xformFactory = (OraTransformationFactory)m_dmeConn.getFactory(
        "oracle.dmt.jdm.transform.OraTransformation" );
  }

  /**
   *   This method illustrates how to build a mining model using 
   * NB_BINNED_DATA_BUILD_JDM dataset and classification settings with 
   * the NB algorithm. 
   * 
   *    By default, the NB algorithm uses 0.01 value for both singleton threshold  
   * and pairwise threshold settings.  
   * 
   * @exception JDMException if model build failed
   */
  public static void buildModel() throws JDMException 
  {
      System.out.println("---------------------------------------------------");    
      System.out.println("--- Build Model - using prior probabilities     ---");
      System.out.println("---------------------------------------------------");
    // 1. Create & save PhysicalDataSpecification      
      PhysicalDataSet buildData = m_pdsFactory.create("MINING_DATA_BUILD_V", false);
      PhysicalAttribute pa = m_paFactory.create("CUST_ID", 
        AttributeDataType.integerType, PhysicalAttributeRole.caseId );
      buildData.addAttribute(pa);
      m_dmeConn.saveObject("nbBuildData_jdm", buildData, true);
    // 2. Create & save Mining Function Settings
      // Create NB algorithm settings
      NaiveBayesSettings nbAlgo = m_nbFactory.create();
      //
      // Thresholding probabilities
      // To increase the accuracy of its estimates, ODM can optionally eliminate
      // probabilities whose estimates are based on relatively small amounts of 
      // data.
      //
      // The Singleton fraction refers to the fraction of training data in which 
      // a predictor, e.g., X, takes on a specific value, e.g., x1. 
      // 
      // Pairwise fraction refers to the fraction of training data in which a 
      // predictor, X, takes on a specific value, x1, **when** the target, T,
      // takes on a specific value t1.
      //
      // If the singleton fraction associated with a predictor value or the 
      // pairwise fraction associated with a (predictor value, target value)
      // pair are below certain respective thresholds, they are ignored 
      // in the probability calculations.
      //
      // Examples settings are:
      // nbAlgo.setSingletonThreshold(0.01f);
      // nbAlgo.setPairwiseThreshold(0.0001f);
      //
      // Create ClassificationSettings
      ClassificationSettings buildSettings = m_clasFactory.create();
      buildSettings.setAlgorithmSettings(nbAlgo);
      buildSettings.setTargetAttributeName("AFFINITY_CARD");
      ((OraBuildSettings)buildSettings).useAutomatedDataPreparations(true);//NEW 11g capability of Automated Data Preparation
      //
      // Set target prior probabilities.  
      // 
      // The priors represent the overall 
      // distribution of the target in the population. By default, the priors 
      // are computed from the sample (in this case, the build data). If the 
      // sample is known to be a distortion of the population target 
      // distribution (because, say, stratified sampling has been employed, or 
      // due to some other reason), then the user can override the default by 
      // providing prior probabilities as a setting for model creation. 
      // See Oracle Data Mining Concepts Guide for more details.
      Map priorMap = new HashMap();
      priorMap.put(new Double(0), new Double(0.65));
      priorMap.put(new Double(1), new Double(0.35));
      buildSettings.setPriorProbabilitiesMap("AFFINITY_CARD", priorMap);
      m_dmeConn.saveObject("nbBuildSettings_jdm", buildSettings, true);
    //Turn Off ADP for CUST_GENDER attribute
     //Create TransformationSequence
      OraExpressionTransform exprXform = m_xformFactory.createExpressionTransform();
       //When attribute specification is set to NOPREP, ADP is truned off for that attribute.
       //Here we are specifying for CUST_GENDER attribute "NOPREP" to flag ADP to be truned off
      exprXform.addAttributeExpression("CUST_GENDER", null, null, "NOPREP");
      //Output view can be specified a null in this case, because we are 
      //not intended to create a view but embed the expression transformations 
      //with the model
      OraTransformationSequence xformSeq = m_xformFactory.createTransformationSequence(
       "MINING_DATA_BUILD_V", exprXform, null ); 
      m_dmeConn.saveObject("nbTSADPoff_jdm", xformSeq, true);
    // 3. Create, save & execute Build Task      
      BuildTask buildTask = m_buildFactory.create(
                     "nbBuildData_jdm", //Build data specification
                     "nbBuildSettings_jdm", //Mining function settings name
                     "nbModel_jdm" //Mining model name
                     ); 
      //Specify expression transformations that are defined to specify ADP off attributes 
      //as an embedded transformation to the model build
      ((OraBuildTask)buildTask).setTransformationSequenceName("nbTSADPoff_jdm");
      buildTask.setDescription("nbBuildTask_jdm");
      executeTask(buildTask, "nbBuildTask_jdm"); 
    // 4. Restore the model from the DME and explore the details of the model
      ClassificationModel model = 
        (ClassificationModel)m_dmeConn.retrieveObject(
          "nbModel_jdm", NamedObject.model);
          
      OraExpressionTransform autoDataPrepSQLExprs = ((OraModel)model).getModelTransformations();
      Map exprMap = autoDataPrepSQLExprs.getAttributeExpressionMap();
      
      // Display model build settings
      ClassificationSettings retrievedBuildSettings = 
                               (ClassificationSettings)model.getBuildSettings();
      if(buildSettings == null) 
          System.out.println("Failure to restore build settings.");
      else 
          displayBuildSettings(retrievedBuildSettings, "nbBuildSettings_jdm");
      // Display model signature    
      displayModelSignature((Model)model);
      // Display model details      
      displayNBModelDetails(model);
  }

  /**
   * Create and save cost matrix.  
   * 
   *   Consider an example where it costs $10 to mail a promotion to a
   * prospective customer and if the prospect becomes a customer, the
   * typical sale including the promotion, is worth $100. Then the cost
   * of missing a customer (i.e. missing a $100 sale) is 10x that of
   * incorrectly indicating that a person is a good prospect (spending
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
   *      0             1                0.35
   *      1             0                0.65
   *      1             1                0    
   */
  private static String createCostMatrix() throws JDMException
  {
    String costMatrixName = "nbCostMatrix";
    // Create categorySet
    CategorySet catSet = m_catSetFactory.create(AttributeDataType.integerType);
    // Add category values
    catSet.addCategory(new Integer(0), CategoryProperty.valid);
    catSet.addCategory(new Integer(1), CategoryProperty.valid);
    // Create cost matrix
    CostMatrix costMatrix = m_costMatrixFactory.create(catSet);
    //                  ActualTarget    PredictedTarget  Cost
    //                  ------------    ---------------  ----
    costMatrix.setValue(new Integer(0), new Integer(0),  0);
    costMatrix.setValue(new Integer(0), new Integer(1),  0.35f);
    costMatrix.setValue(new Integer(1), new Integer(0),  0.65f);
    costMatrix.setValue(new Integer(1), new Integer(1),  0);
    //save cost matrix
    m_dmeConn.saveObject(costMatrixName, costMatrix, true);
    return costMatrixName;
  }
  
  /**
   *   This method illustrates how to test the mining model with 
   * NB_BINNED_DATA_TEST_JDM dataset using classification test task. After
   * completion of the task, a classification test metrics object is created
   * in the DMS. The classification test metrics object encapsulates 
   * different metrics like accuracy, confusion matrix, lift, and ROC. 
   * 
   * @param testResultName test result name
   * @param useCostMatrix flag to indicate the enabling/disabling cost matrix added to the model
   * @exception JDMException if model test failed
   */
  public static void testModel(String testResultName, boolean useCostMatrix) 
    throws JDMException 
  {
    if (useCostMatrix) {
      System.out.println("---------------------------------------------------");    
      System.out.println("--- Test Model - using test input table         ---");
      System.out.println("---            - using cost matrix table        ---");
      System.out.println("---------------------------------------------------");
    }
    else {
      System.out.println("---------------------------------------------------");    
      System.out.println("--- Test Model - using test input table         ---");
      System.out.println("---            - using no cost matrix table     ---");
      System.out.println("---------------------------------------------------");
    }
    // 1. Create & save PhysicalDataSpecification      
      PhysicalDataSet testData = m_pdsFactory.create(
        "MINING_DATA_TEST_V", false );
      PhysicalAttribute pa = m_paFactory.create("CUST_ID", 
        AttributeDataType.integerType, PhysicalAttributeRole.caseId );
      testData.addAttribute( pa );
      m_dmeConn.saveObject( "nbTestData_jdm", testData, true );
    // 2. Create, store & execute Test Task
      ClassificationTestTask testTask = m_testFactory.create( 
        "nbTestData_jdm", "nbModel_jdm", testResultName );
      testTask.setPositiveTargetValue(new Integer(1));
      testTask.setNumberOfLiftQuantiles(10);
      //Set useCost flag to indicate enabling/disabling of cost matrix in testing the model
      ((OraTestTask)testTask).useCost(useCostMatrix);
      // Store & execute the task
      boolean isTaskSuccess = executeTask(testTask, "nbTestTask_jdm"); 
      if( isTaskSuccess ) {
        // Restore & display test metrics
        ClassificationTestMetrics testMetrics = 
          (ClassificationTestMetrics)m_dmeConn.retrieveObject( 
            testResultName, 
            NamedObject.testMetrics );
        displayTestMetricDetails(testMetrics);
      }
  }

  /**
   *   This method illustrates how to apply the mining model on 
   * NB_BINNED_DATA_APPLY_JDM dataset to predict customer
   * response. After completion of the task, an apply output table with the
   * predicted results is created at the user specified location. 
   * 
   * @exception JDMException if model apply failed
   */ 
  public static void applyModel() throws JDMException
  {
      System.out.println("---------------------------------------------------");    
      System.out.println("--- Apply Model                                 ---");
      System.out.println("---------------------------------------------------");
      // 1. Create & save PhysicalDataSpecification      
      PhysicalDataSet applyData = m_pdsFactory.create(
        "MINING_DATA_APPLY_V", false );
      PhysicalAttribute pa = m_paFactory.create("CUST_ID", 
        AttributeDataType.integerType, PhysicalAttributeRole.caseId );
      applyData.addAttribute( pa );
      m_dmeConn.saveObject( "nbApplyData_jdm", applyData, true );
      // 2. Create & save ClassificationApplySettings
      ClassificationApplySettings clasAS = m_applySettingsFactory.create();
      // Example: Add source attributes
      // HashMap sourceAttrMap = new HashMap();
      // sourceAttrMap.put( "AGE", "AGE" );
      // sourceAttrMap.put( "OCCUPATION", "OCCUPATION" );
      // clasAS.setSourceDestinationMap( sourceAttrMap );
      m_dmeConn.saveObject( "nbApplySettings_jdm", clasAS, true);  
      // 3. Create, store & execute apply Task
      DataSetApplyTask applyTask = m_dsApplyFactory.create(
                                  "nbApplyData_jdm", "nbModel_jdm", 
                                  "nbApplySettings_jdm", "NB_APPLY_OUTPUT_JDM");
      executeTask(applyTask, "nbApplyTask_jdm");
     // 4. Display apply result -- Note that APPLY results do not need to be 
     //    reverse transformed, as done in the case of model details. This is 
     //    because class values of a classification target were not (required to 
     //    be) binned or normalized.
     displayTable("NB_APPLY_OUTPUT_JDM", 
                  "where ROWNUM < 11", 
                  "order by CUST_ID");
  } 

  /**
   *   This method illustrates how to apply the mining model (mapped by rank) on 
   * NB_BINNED_DATA_APPLY_JDM dataset to predict customer
   * response. After completion of the task, an apply output table with the
   * predicted results is created at the user specified location. 
   * 
   * @param costMatrixName table name of the supplied cost matrix
   * @exception JDMException if model apply failed
   */ 
  public static void rankApplyModel(String costMatrixName) throws JDMException
  {
      System.out.println("---------------------------------------------------");    
      System.out.println("--- Rank Apply Model                            ---");
      System.out.println("---------------------------------------------------");
      // 1. Create & save PhysicalDataSpecification -- We will use apply result 
      //    table from applyModel() to perform rank apply here.
      PhysicalDataSet applyData = m_pdsFactory.create(
        "MINING_DATA_APPLY_V", false );
      PhysicalAttribute pa = m_paFactory.create("CUST_ID", 
        AttributeDataType.integerType, PhysicalAttributeRole.caseId );
      applyData.addAttribute( pa );
      m_dmeConn.saveObject( "nbRankApplyData_jdm", applyData, true );
      // 2. Create & save ClassificationApplySettings
      ClassificationApplySettings clasAS = m_applySettingsFactory.create();
      if(costMatrixName != null ) {
        OraClassificationModel.addCostMatrix(m_dmeConn, "nbModel_jdm", costMatrixName);       
        ((OraClassificationApplySettings)clasAS).useCostMatrixFromModel(true);
      }
      int topNRanks = 2;  
      String[] topNCategoryColNames = new String[topNRanks];
      String[] topNProbabilityColNames = new String[topNRanks];
      String[] topNCostColNames = new String[topNRanks];
      for(int i=0; i<topNRanks; i++) 
      {
        topNCategoryColNames[i]="TOPCAT_" + (i+1);
        topNProbabilityColNames[i] = "TOPPROB_" + (i+1);
        topNCostColNames[i] = "TOPCOST_" + (i+1);
      }
      clasAS.mapByRank(ClassificationApplyContent.predictedCategory, topNCategoryColNames, true );
      clasAS.mapByRank(ClassificationApplyContent.probability,topNProbabilityColNames,true);
      if(costMatrixName != null)
        clasAS.mapByRank(ClassificationApplyContent.cost,topNCostColNames,true);
      m_dmeConn.saveObject( "nbRankApplySettings_jdm", clasAS, true);
      // 3. Create, store & execute apply Task
      DataSetApplyTask applyTask = m_dsApplyFactory.create(
                                  "nbRankApplyData_jdm", "nbModel_jdm", 
                                  "nbRankApplySettings_jdm", "NB_RANK_APPLY_OUTPUT_JDM");
      executeTask(applyTask, "nbRankApplyTask_jdm");
      // 4. Display apply result -- Note that APPLY results do not need to be 
      //    reverse transformed, as done in the case of model details. This is 
      //    because class values of a classification target were not (required to 
      //    be) binned or normalized.
      displayTable("NB_RANK_APPLY_OUTPUT_JDM", 
                  "where ROWNUM < 11", 
                  "order by CUST_ID");
  }
  
  /**
   * This method stores the given task with the specified name in the DMS 
   * and submits the task for asynchronous execution in the DMS. After 
   * completing the task successfully it returns true. If there is a task 
   * failure, then it prints the error description and returns false.
   * 
   * @param taskObj task object
   * @param taskName name of the task
   * 
   * @return boolean returns true when the task is successful
   * @exception JDMException if task execution failed
   */
  public static boolean executeTask(Task taskObj, String taskName)
    throws JDMException {
    boolean isTaskSuccess = false;
    m_dmeConn.saveObject(taskName, taskObj, true);
    ExecutionHandle execHandle = m_dmeConn.execute(taskName);
    System.out.print(taskName + " is started, please wait. ");
    //Wait for completion of the task
    ExecutionStatus status = execHandle.waitForCompletion(Integer.MAX_VALUE);    
    //Check the status of the task after completion
    isTaskSuccess = status.getState().equals(ExecutionState.success);
    if( isTaskSuccess ) {
      //Task completed successfully
      System.out.println(taskName + " is successful.");
    } else {//Task failed
      System.out.println(taskName + " is failed.\nFailure Description: " + 
        status.getDescription() );
    }
    return isTaskSuccess;
  }

 private static void displayBuildSettings(
    ClassificationSettings clasSettings, String buildSettingsName) 
  {
    System.out.println("BuildSettings Details from the " 
      + buildSettingsName + " table:");
      displayTable(buildSettingsName, "", "order by SETTING_NAME");
    System.out.println("BuildSettings Details from the " 
      + buildSettingsName + " model build settings object:");
    String objName = clasSettings.getName();
    if(objName != null)
      System.out.println("Name = " + objName);
    String objDescription = clasSettings.getDescription();
    if(objDescription != null)
      System.out.println("Description = " + objDescription);
    java.util.Date creationDate = clasSettings.getCreationDate();
    String creator = clasSettings.getCreatorInfo();     
    boolean isADP_ON = ((OraBuildSettings)clasSettings).useAutomatedDataPreparations();
    System.out.println("Use Automated Data Preparations: " + isADP_ON);
    String targetAttrName = clasSettings.getTargetAttributeName();
    System.out.println("Target attribute name = " + targetAttrName);
    AlgorithmSettings algoSettings = clasSettings.getAlgorithmSettings();
    if(algoSettings == null)
      System.out.println("Failure: clasSettings.getAlgorithmSettings() returns null");
    MiningAlgorithm algo = algoSettings.getMiningAlgorithm();
    if(algo == null) System.out.println("Failure: algoSettings.getMiningAlgorithm() returns null");
    System.out.println("Algorithm Name: " + algo.name());
    MiningFunction function = clasSettings.getMiningFunction();
    if(function == null) System.out.println("Failure: clasSettings.getMiningFunction() returns null");
    System.out.println("Function Name: " + function.name());
    try {
      Map map = clasSettings.getPriorProbabilitiesMap(targetAttrName);
      if(map != null)    System.out.println("Priors Map: " + map.toString());
    } catch(Exception jdmExp) 
    {
      System.out.println("Failure: clasSettings.getPriorProbabilitiesMap(targetAttrName)throws exception");
      jdmExp.printStackTrace();
    }    
  }

  /**
   * This method displayes the NB model signature.
   * 
   * @param model model object
   * @exception JDMException if failed to retrieve model signature
   */  
  public static void displayModelSignature(Model model) throws JDMException
  {
    String modelName = model.getName();
    System.out.println("Model Name: " + modelName);
    ModelSignature modelSignature = model.getSignature();
    System.out.println("ModelSignature Deatils: ( Attribute Name, Attribute Type )");
    MessageFormat mfSign = new MessageFormat("                        ( {0}, {1} )");
    String[] vals = new String[3]; 
    Collection sortedSet = modelSignature.getAttributes();
    Iterator attrIterator = sortedSet.iterator();
    while(attrIterator.hasNext()) 
    {
      SignatureAttribute attr = (SignatureAttribute)attrIterator.next();
      vals[0] = attr.getName();
      vals[1] = attr.getDataType().name();
      System.out.println( mfSign.format(vals) );
    }
  }

  /**
   * This method displayes the NB model details.
   * 
   * @param nbModelDetails nb model details object
   * @exception JDMException if failed to retrieve model details
   */  
  public static void displayNBModelDetails(
    ClassificationModel model) 
    throws JDMException
  {
    // Retrieve Oracle NB model deatils extensions
    NaiveBayesModelDetail nbModelDetails = 
        (NaiveBayesModelDetail)model.getModelDetail();
    //Define message formats to display the nb model details contents
    System.out.println("NBModelDetails: ( Target Attribute Name, Target Attribute Value, Prior Probability )");
    MessageFormat mfTarget = new MessageFormat("                ( {0}, {1}, {2} )");
    MessageFormat mfConditinals = new MessageFormat("                     ( {0}, {1}, {2} )");
    // Get target values from the model
    String targetAttributeName = model.getTargetAttributeName();
    CategorySet targetCategorySet = model.getTargetCategorySet();
    Object[] targetVals = targetCategorySet.getValues();
    double[] targetProbabilities = null;//aka Prior Probability
    if(targetVals != null) {
      targetProbabilities = new double[targetVals.length];
      for(int ixTargetVal=0; ixTargetVal < targetVals.length; ixTargetVal++) 
      {
        targetProbabilities[ixTargetVal] = nbModelDetails.getTargetProbability(targetVals[ixTargetVal]);
      }
    
      for(int i=0; i < targetVals.length; i++) {
        //print top conditionals for each target attribute value
        int j=0; 
        String[] targetDetails = new String[3];
        targetDetails[0] = targetAttributeName;
        targetDetails[1] = targetVals[i].toString();
        targetDetails[2] = m_df.format( targetProbabilities[i] );          
        System.out.println( mfTarget.format(targetDetails));
        //Get the signature attributes
        ModelSignature modelSignature = model.getSignature();      
        Collection sortedSet = modelSignature.getAttributes();
        Iterator attrIterator = sortedSet.iterator();
        System.out.println("                     ( AttributeName, AttributeValue, PairProbability )");
        while(attrIterator.hasNext()) 
        {
          SignatureAttribute attr = (SignatureAttribute)attrIterator.next();
          String attrName = attr.getName();
          Map pairProbabilitisMap = 
            ((OraNaiveBayesModelDetail)nbModelDetails).getPairProbabilities(attrName, targetVals[i]);          
          Object[] attrVals = pairProbabilitisMap.keySet().toArray();
          if(attrVals != null) {
            for(int iAttr=0; iAttr < attrVals.length; iAttr++) 
            {
              String[] pairProbabilityDetails = new String[3];            
              pairProbabilityDetails[0] = attrName;
              pairProbabilityDetails[1] = attrVals[iAttr].toString();
              pairProbabilityDetails[2] = null;
              Number pairProbability = (Number)pairProbabilitisMap.get(attrVals[iAttr]);
              if(pairProbability != null)
                pairProbabilityDetails[2]= m_df.format( pairProbability.doubleValue() );
              System.out.println(mfConditinals.format(pairProbabilityDetails));
            }
          }
          
        }      
      }
    }
  }

  /**
   * Display classification test metrics object
   * 
   * @param testMetrics classification test metrics object
   * @exception JDMException if failed to retrieve test metric details
   */
  public static void displayTestMetricDetails(
    ClassificationTestMetrics testMetrics) throws JDMException
  {
     //Retrieve Oracle NB model test metrics deatils extensions
    // Test Metrics Name
    System.out.println("Test Metrics Name = " + testMetrics.getName());  
    // Model Name
    System.out.println("Model Name = " + testMetrics.getModelName());
    // Test Data Name
    System.out.println("Test Data Name = " + testMetrics.getTestDataName());
    // Accuracy
    System.out.println("Accuracy = " + m_df.format(testMetrics.getAccuracy().doubleValue()));
    // Confusion Matrix
    ConfusionMatrix confusionMatrix = testMetrics.getConfusionMatrix();
    Collection categories = confusionMatrix.getCategories(); 
    Iterator xIterator = categories.iterator();
    System.out.println("Confusion Matrix: Accuracy = " + m_df.format(confusionMatrix.getAccuracy()));
    System.out.println("Confusion Matrix: Error = " + m_df.format(confusionMatrix.getError()));
    System.out.println("Confusion Matrix:( Actual, Predicted, Value )");
    MessageFormat mf = new MessageFormat("                 ( {0}, {1}, {2} )");
    String[] vals = new String[3];    
    while(xIterator.hasNext()) 
    {
      Object actual = xIterator.next();
      vals[0] = actual.toString();
      Iterator yIterator = categories.iterator();
      while(yIterator.hasNext()) 
      {
        Object predicted = yIterator.next();
        vals[1] = predicted.toString();
        long number = confusionMatrix.getNumberOfPredictions(actual, predicted);
        vals[2] = Long.toString(number);
        System.out.println(mf.format(vals));
      }
    }
    // Lift
    Lift lift = testMetrics.getLift();
    System.out.println("Lift Details:");
    System.out.println("Lift: Target Attribute Name = " + lift.getTargetAttributeName());
    System.out.println("Lift: Positive Target Value = " + lift.getPositiveTargetValue());
    System.out.println("Lift: Total Cases  = " + lift.getTotalCases());
    System.out.println("Lift: Total Positive Cases = " + lift.getTotalPositiveCases());
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
    System.out.println("ROC: Area Under Curve = " + m_df.format(roc.getAreaUnderCurve()));
    int nROCThresh = roc.getNumberOfThresholdCandidates();
    System.out.println("ROC: Number Of Threshold Candidates = " + nROCThresh);
    System.out.println("ROC: ( INDEX, PROBABILITY, TRUE_POSITIVES, FALSE_NEGATIVES, FALSE_POSITIVES, TRUE_NEGATIVES, TRUE_POSITIVE_FRACTION, FALSE_POSITIVE_FRACTION )");
    MessageFormat mfROC = new MessageFormat("     ( {0}, {1}, {2}, {3}, {4}, {5}, {6}, {7} )");
    String[] rocVals = new String[8];
    for(int iROC=1; iROC <= nROCThresh; iROC++) 
    {
      rocVals[0] = Integer.toString(iROC); //INDEX
      rocVals[1] = m_df.format(roc.getProbabilityThreshold(iROC));//PROBABILITY
      rocVals[2] = Long.toString(roc.getPositives(iROC, true));//TRUE_POSITIVES
      rocVals[3] = Long.toString(roc.getNegatives(iROC, false));//FALSE_NEGATIVES
      rocVals[4] = Long.toString(roc.getPositives(iROC, false));//FALSE_POSITIVES
      rocVals[5] = Long.toString(roc.getNegatives(iROC, true));//TRUE_NEGATIVES
      rocVals[6] = m_df.format(roc.getHitRate(iROC));//TRUE_POSITIVE_FRACTION
      rocVals[7] = m_df.format(roc.getFalseAlarmRate(iROC));//FALSE_POSITIVE_FRACTION     
      System.out.println(mfROC.format(rocVals));
    }
  }

  private static void displayTable(String tableName, String whereCause, String orderByColumn) 
  {
    StringBuffer emptyCol = new StringBuffer("                    ");
    
    java.sql.Connection dbConn = 
      ((OraConnection)m_dmeConn).getDatabaseConnection();
    PreparedStatement pStmt = null;
    ResultSet rs = null;
    try 
    {
      pStmt = dbConn.prepareStatement("SELECT * FROM " + tableName + " " + whereCause + " " + orderByColumn);
      rs = pStmt.executeQuery();      
      ResultSetMetaData rsMeta = rs.getMetaData();
      int colCount = rsMeta.getColumnCount();
      StringBuffer header = new StringBuffer();
      System.out.println("Table : " + tableName);
      //Build table header
      for(int iCol=1; iCol<=colCount; iCol++) 
      {
        String colName = rsMeta.getColumnName(iCol);
        header.append(emptyCol.replace(0, colName.length(), colName));
        emptyCol = new StringBuffer("                    ");
      }
      System.out.println(header.toString());
      //Write table data
      while(rs.next()) 
      {
        StringBuffer rowContent = new StringBuffer();
        for(int iCol=1; iCol<=colCount; iCol++) 
        {
          int sqlType = rsMeta.getColumnType(iCol);          
          Object obj = rs.getObject(iCol);
          String colContent = null;
          
          if(obj instanceof java.lang.Number) 
          {
            try 
            {
              BigDecimal bd = (BigDecimal)obj;
              if(bd.scale() > 5) 
              {
                colContent = m_df.format(obj);
              } else 
              {
                colContent = bd.toString();
              }
            } catch(Exception anyExp) {
              colContent = m_df.format(obj);
            }
          } else 
          {
            if(obj == null)
              colContent = "NULL";
            else
              colContent = obj.toString();
          }
          
          rowContent.append(" "+emptyCol.replace(0, colContent.length(), colContent));
          emptyCol = new StringBuffer("                    ");
        }
        System.out.println(rowContent.toString());
      }
    } catch(Exception anySqlExp) {
      anySqlExp.printStackTrace();
    }//Ignore
  }
  
  private static void clean() 
  {
    java.sql.Connection dbConn = 
      ((OraConnection)m_dmeConn).getDatabaseConnection();
    Statement stmt = null;    
    //Drop apply output table
    try 
    {
      stmt = dbConn.createStatement();
      stmt.executeUpdate("DROP TABLE NB_APPLY_OUTPUT_JDM"); 
    } catch(Exception anySqlExp) {} // Ignore
    finally{
      try {
        stmt.close();   
      }
      catch(Exception anySqlExp){}
    }
    //Drop rank apply output table
    try 
    {
      stmt = dbConn.createStatement();
      stmt.executeUpdate("DROP TABLE NB_RANK_APPLY_OUTPUT_JDM"); 
    } catch(Exception anySqlExp) {} // Ignore    
    finally{
      try {
        stmt.close();   
      }
      catch(Exception anySqlExp){}
    }
    //Drop the model
    try {
      m_dmeConn.removeObject("nbModel_jdm", NamedObject.model);
    } catch(Exception jdmExp) {}
    //Drop the test metrics
    try {
      m_dmeConn.removeObject("nbTestMetricsWithCost_jdm", NamedObject.testMetrics);
    } catch(JDMException jdmExp) {}
    try {
      m_dmeConn.removeObject("nbTestMetrics_jdm", NamedObject.testMetrics);
    } catch(JDMException jdmExp) {}
  }

}   
