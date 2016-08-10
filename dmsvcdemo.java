// Copyright (c) 2004, 2008, Oracle. All rights reserved.
// File: dmsvcdemo.java
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

import javax.datamining.ExecutionState;
import javax.datamining.ExecutionStatus;
import javax.datamining.JDMException;
import javax.datamining.MiningAlgorithm;
import javax.datamining.MiningFunction;
import javax.datamining.NamedObject;
import javax.datamining.algorithm.svm.KernelFunction;
import javax.datamining.algorithm.svm.classification.SVMClassificationSettings;
import javax.datamining.algorithm.svm.classification.SVMClassificationSettingsFactory;
import javax.datamining.base.AlgorithmSettings;
import javax.datamining.base.Model;
import javax.datamining.base.Task;
import javax.datamining.data.AttributeDataType;
import javax.datamining.data.CategorySet;
import javax.datamining.data.ModelSignature;
import javax.datamining.data.PhysicalAttribute;
import javax.datamining.data.PhysicalAttributeFactory;
import javax.datamining.data.PhysicalAttributeRole;
import javax.datamining.data.PhysicalDataSet;
import javax.datamining.data.PhysicalDataSetFactory;
import javax.datamining.data.SignatureAttribute;
import javax.datamining.modeldetail.svm.SVMClassificationModelDetail;
import javax.datamining.resource.ConnectionSpec;
import javax.datamining.supervised.classification.ClassificationApplySettings;
import javax.datamining.supervised.classification.ClassificationApplySettingsFactory;
import javax.datamining.supervised.classification.ClassificationModel;
import javax.datamining.supervised.classification.ClassificationSettings;
import javax.datamining.supervised.classification.ClassificationSettingsFactory;
import javax.datamining.supervised.classification.ClassificationTestMetrics;
import javax.datamining.supervised.classification.ClassificationTestTask;
import javax.datamining.supervised.classification.ClassificationTestTaskFactory;
import javax.datamining.supervised.classification.ConfusionMatrix;
import javax.datamining.supervised.classification.Lift;
import javax.datamining.supervised.classification.ReceiverOperatingCharacterics;
import javax.datamining.task.BuildTask;
import javax.datamining.task.BuildTaskFactory;
import javax.datamining.task.apply.DataSetApplyTask;
import javax.datamining.task.apply.DataSetApplyTaskFactory;

import oracle.dmt.jdm.algorithm.svm.classification.OraSVMClassificationSettings;
import oracle.dmt.jdm.base.OraBuildSettings;
import oracle.dmt.jdm.base.OraTask;
import oracle.dmt.jdm.modeldetail.svm.OraSVMClassificationModelDetail;
import oracle.dmt.jdm.resource.OraConnection;
import oracle.dmt.jdm.resource.OraConnectionFactory;
import oracle.dmt.jdm.supervised.classification.OraClassificationModel;
import oracle.dmt.jdm.supervised.classification.OraLift;

/**
 *   This demo program describes how to use the Oracle Data Mining (ODM) Java API
 * to solve a classification problem using Support Vector Machines (SVM)
 * algorithm.
 * ------------------------------------------------------------------------------
 *                             PROBLEM DEFINITION
 * ------------------------------------------------------------------------------
 *   How to predict whether a customer responds or not to the new affinity card
 * program using a classifier based on SVM algorithm?
 * ------------------------------------------------------------------------------
 *                             DATA DESCRIPTION
 * ------------------------------------------------------------------------------
 *   Data for this demo is composed from base tables in the Sales History (SH)
 * Schema. The SH schema is an Oracle Database Sample Schema that has the customer
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
 *   Build Model:
 *     Mining Model is the prime object in data mining. The buildModel() method
 *     illustrates how to build a classification model using SVM algorithm.
 *
 *   Test Model:
 *     Classification models' performance can be evaluated by computing test
 *     metrics like accuracy, confusion matrix, lift and ROC. The testModel() or
 *     computeTestMetrics() method illustrates how to perform a test operation to
 *     compute various metrics.
 *
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
 
public class dmsvcdemo extends Object {
  // Connection related data members
  private static javax.datamining.resource.Connection m_dmeConn;
  private static javax.datamining.resource.ConnectionFactory m_dmeConnFactory;
  // Object factories used in this demo program
  private static PhysicalDataSetFactory m_pdsFactory;
  private static PhysicalAttributeFactory m_paFactory;
  private static ClassificationSettingsFactory m_clasFactory;
  private static SVMClassificationSettingsFactory m_svmcFactory;
  private static BuildTaskFactory m_buildFactory;
  private static DataSetApplyTaskFactory m_dsApplyFactory;
  private static ClassificationTestTaskFactory m_testFactory;
  private static ClassificationApplySettingsFactory m_applySettingsFactory;
  // Global constants 
  private static DecimalFormat m_df = new DecimalFormat("##.####");
  private static boolean displayModelDeatils = false;//By default it is set to false. To view the details of the model this flag can be enabled
  

  public static void main( String args[] ) { 
    try { 
        if (( args.length != 0 ) & ( args.length != 3 )) {
          System.out.println("Usage: java dmsvcdemo ");
          System.out.println("   or: java dmsvcdemo <Host name>:<Port>:<SID> <User Name> <Password>");
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
        // 4. Build a model with supplied prior probability
        buildModel();
        // 5. Test model - To compute accuracy and confusion matrix, lift result
        //                 and ROC for the model.
        testModel();
        // 6. Apply the model
        applyModel();
        // 7. Start execution of the first task in the sequnece
        m_dmeConn.execute("svmcBuildTask_jdm");
        // 8. Monitor and display results (if any)
        monitorTaskExecutionProcess();
        
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
    m_svmcFactory = (SVMClassificationSettingsFactory)m_dmeConn.getFactory(
      "javax.datamining.algorithm.svm.classification.SVMClassificationSettings");
    m_buildFactory = (BuildTaskFactory)m_dmeConn.getFactory(
      "javax.datamining.task.BuildTask");
    m_testFactory = (ClassificationTestTaskFactory)m_dmeConn.getFactory(
      "javax.datamining.supervised.classification.ClassificationTestTask");
    m_dsApplyFactory = (DataSetApplyTaskFactory)m_dmeConn.getFactory(
      "javax.datamining.task.apply.DataSetApplyTask");
    m_applySettingsFactory = (ClassificationApplySettingsFactory)m_dmeConn.getFactory(
      "javax.datamining.supervised.classification.ClassificationApplySettings");
  }
  
  /**
   *   This method illustrates how to build mining model using 
   * MINING_DATA_BUILD_V dataset and classification settings with 
   * SVM algorithm. 
   * 
   *    By default SVM algorithm chooses an kernel type automatically. This 
   * choice can be overriden by the user. Linear kernel is preferred high 
   * dimensional data, and Gaussian kernel for low dimensional data. Here we use 
   * linear kernel to demonstrate the getModelDetail() API, which applies only 
   * for models.  
   * 
   * @exception JDMException if model build failed
   */
  public static void buildModel() throws JDMException 
  {      
     // 1. Create & save PhysicalDataSpecification 
      PhysicalDataSet buildData = 
                        m_pdsFactory.create("MINING_DATA_BUILD_V", false);
      PhysicalAttribute pa = m_paFactory.create("CUST_ID", 
        AttributeDataType.integerType, PhysicalAttributeRole.caseId );
      buildData.addAttribute(pa);
      m_dmeConn.saveObject("svmcBuildData_jdm", buildData, true);
     // 2. Create & save Mining Function Settings
      // Create SVMC algorithm settings
      SVMClassificationSettings svmcAlgo = m_svmcFactory.create();
      svmcAlgo.setKernelFunction(KernelFunction.kLinear);
      // Examples settings are:
      // svmcAlgo.setKernelFunction(KernelFunction.kGaussian);
      // svmcAlgo.setComplexityFactor(0.01f);
      // svmcAlgo.setTolerance(0.01f);
      //
      //Create ClassificationSettings
      ClassificationSettings buildSettings = m_clasFactory.create();
      buildSettings.setAlgorithmSettings(svmcAlgo);
      buildSettings.setTargetAttributeName("AFFINITY_CARD");
      //
      // Set target prior probabilities.  
      // 
      // A priors table is used to influence the weighting of target classes
      // during model creation. SVM's usage of priors is different from the 
      // priors interpretation in probabilistic models. In probabilistic models,
      // priors can be used to correct for biased sampling procedures. Instead,
      // in SVM priors act as a weight vector that biases optimization and favors
      // one class over the other. For example, priors of (0.9, 0.1) for a binary
      // problem specify that an error in the first class has significantly
      // higher penalty that an error in the second class. Priors of (0.5, 0.5)
      // do not introduce a differential weight and would produce the same
      // model as when no priors are provided.
      Map priorMap = new HashMap();
      priorMap.put(new Double(0), new Double(0.35));
      priorMap.put(new Double(1), new Double(0.65));
      buildSettings.setPriorProbabilitiesMap("AFFINITY_CARD", priorMap);
      //Set auto data preparation on
      ((OraBuildSettings)buildSettings).useAutomatedDataPreparations(true);
      m_dmeConn.saveObject("svmcBuildSettings_jdm", buildSettings, true);
     // 3. Create, save & execute Build Task      
      BuildTask buildTask = m_buildFactory.create(
                     "svmcBuildData_jdm", //Build data specification
                     "svmcBuildSettings_jdm", //Mining function settings name
                     "svmcModel_jdm" //Mining model name
                     );                          
      buildTask.setDescription("svmcBuildTask_jdm");
      saveTask(buildTask, "svmcBuildTask_jdm", null);     
  }


  /**
   *   This method illustrates how to compute test metrics using 
   * MINING_DATA_TEST_V dataset and ClassificationTestTask.    
   * @exception JDMException if model test failed
   */
  public static void testModel() throws JDMException
  {       
    // 1. Do the apply on test data to create an apply output table
      // Create & save PhysicalDataSpecification      
        PhysicalDataSet applyData = m_pdsFactory.create(
          "MINING_DATA_TEST_V", false );
        PhysicalAttribute pa = m_paFactory.create("CUST_ID", 
          AttributeDataType.integerType, PhysicalAttributeRole.caseId );
        applyData.addAttribute( pa );
        m_dmeConn.saveObject( "svmcTestData_jdm", applyData, true );
             
        // 5. Create a ClassificationTestTask
         ClassificationTestTask testTask = m_testFactory.create( 
           "svmcTestData_jdm", "svmcModel_jdm", "svcTestMetrics_jdm" );
         testTask.setPositiveTargetValue(new Integer(1));
         testTask.setNumberOfLiftQuantiles(10);
        // 6. Store & execute the task
        saveTask(testTask, "svmcTestTask_jdm", "svmcBuildTask_jdm");             
  } 
  
  /**
   *   This method illustrates how to apply the mining model on the
   * MINING_DATA_APPLY_V dataset to predict customer
   * response. After completion of the task, the apply output table with the
   * predicted results is created at the user specified location. 
   * 
   * @exception JDMException if model apply failed
   */ 
  public static void applyModel() throws JDMException
  {
    // 1. Create & save PhysicalDataSpecification
      PhysicalDataSet applyData = m_pdsFactory.create(
                                            "MINING_DATA_APPLY_V", false );
      PhysicalAttribute pa = m_paFactory.create("CUST_ID", 
        AttributeDataType.integerType, PhysicalAttributeRole.caseId );
      applyData.addAttribute( pa );
      m_dmeConn.saveObject( "svmcApplyData_jdm", applyData, true );
    // 2. Create & save ClassificationApplySettings
      ClassificationApplySettings clasAS = m_applySettingsFactory.create();
      m_dmeConn.saveObject( "svmcApplySettings_jdm", clasAS, true);           
    // 3. Create, store & execute apply Task
     DataSetApplyTask applyTask = m_dsApplyFactory.create(
                              "svmcApplyData_jdm", "svmcModel_jdm", 
                              "svmcApplySettings_jdm", "SVMC_APPLY_OUTPUT_JDM");
     saveTask(applyTask, "svmcApplyTask_jdm", "svmcBuildTask_jdm" );     
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
      //BuildTask
      System.out.println("---------------------------------------------------");
      System.out.println("--- Build Model                                 ---");
      System.out.println("---------------------------------------------------");
      //1. Wait for the completion of the task
      System.out.print("Waiting for the completion of svmcBuildTask_jdm. ");
      ExecutionStatus buildTaskCompletionStatus =
        m_dmeConn.getLastExecutionHandle("svmcBuildTask_jdm").waitForCompletion(Integer.MAX_VALUE);
      //2. If successful
      if (ExecutionState.success.equals(buildTaskCompletionStatus.getState()))
      {
        System.out.println("It is successful. ");
        // 3. Restore the model from the data mining server
        ClassificationModel model =
          (ClassificationModel) m_dmeConn.retrieveObject("svmcModel_jdm",
                                                         NamedObject.model);
        // 4. Explore the details of the restored model
        // Display model build settings
        ClassificationSettings retrievedBuildSettings =
          (ClassificationSettings) model.getBuildSettings();
        if (retrievedBuildSettings == null)
          System.out.println("Failure to restore build settings.");
        else
          displayBuildSettings(retrievedBuildSettings,
                               "svmcBuildSettings_jdm");
         // Display model signature    
         displayModelSignature((Model)model);
         if(displayModelDeatils)
           displaySVMCModelDetails((Model)model);

        System.out.println("---------------------------------------------------");
        System.out.println("--- Test Model                                  ---");
        System.out.println("---------------------------------------------------");

        //If model build is successful, then do testData ApplyTask
        //1. Wait for the completion of the task
        System.out.print("Waiting for the completion of svmcTestTask_jdm. ");
        ExecutionStatus testTaskCompletionStatus =
          m_dmeConn.getLastExecutionHandle("svmcTestTask_jdm").waitForCompletion(Integer.MAX_VALUE);
        //2. If successful
        if (ExecutionState.success.equals(testTaskCompletionStatus.getState()))
        {
          System.out.println("It is successful. ");          
          // Restore & display test metrics
          ClassificationTestMetrics testMetrics =
            (ClassificationTestMetrics) m_dmeConn.retrieveObject("svcTestMetrics_jdm",
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
        //ApplyTask(s)
        //Waiting for apply task to complete
        System.out.println("---------------------------------------------------");
        System.out.println("--- Apply Model                                 ---");
        System.out.println("---------------------------------------------------");

        ExecutionStatus applyTaskStatus =
          m_dmeConn.getLastExecutionHandle("svmcApplyTask_jdm").waitForCompletion(Integer.MAX_VALUE);
        if (ExecutionState.success.equals(applyTaskStatus.getState()))
        {
          // Display apply result -- Note that APPLY results do not need to be
          //    reverse transformed, as done in the case of model details. This is
          //    because class values of a classification target were not (required to
          //    be) binned or normalized.
          displayTable("SVMC_APPLY_OUTPUT_JDM", "where ROWNUM < 11",
                       "order by CUST_ID");
        }
        else
        {
          System.out.println("svmcApplyTask_jdm is at state:" +
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

  private static void displayBuildSettings(
    ClassificationSettings clasSettings, String buildSettingsName) 
  {
    // Display build settings table
    System.out.println("BuildSettings Details from the " 
      + buildSettingsName + " table:");
      displayTable(buildSettingsName, "", "order by SETTING_NAME");
    // Display build settings object obtained from the model
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
    // List of SVM algorithm settings availabe in linear kernel
    KernelFunction kernelFunction = ((OraSVMClassificationSettings)algoSettings).getKernelFunction();
    System.out.println("Kernel Function: " + kernelFunction.name());
    double doubleValue = ((OraSVMClassificationSettings)algoSettings).getComplexityFactor();
    System.out.println("Complexity Factor: " + m_df.format(doubleValue));
    doubleValue = ((OraSVMClassificationSettings)algoSettings).getTolerance();
    System.out.println("Tolerance: " + m_df.format(doubleValue));
    boolean enable = ((OraSVMClassificationSettings)algoSettings).getActiveLearning();
    System.out.println("Is Active Learning enabled? " + enable);
  }

  /**
   * This method displayes SVMC model signature.
   * 
   *  @param model model object
   *  @exception JDMException if failed to retrieve model signature
   */
  public static void displayModelSignature(Model model) throws JDMException
  {
    String modelName = model.getName();
    System.out.println("Model Name: " + modelName);
    ModelSignature modelSignature = model.getSignature();
    System.out.println("Model Signature: ( Attribute Name, Attribute Type )");
    MessageFormat mfSign = new MessageFormat("                        ( {0}, {1} )");
    String[] vals = new String[2]; 
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
   * This method displayes SVMC model details.  The coefficient indicates the 
   * relative influence of a given (attribute, value) pair on the target value. 
   * A negative coefficient value indicates a negative influence.
   * 
   * @param svmcModelDetails svm classification model details object
   * @exception JDMException if failed to retrieve model details
   */ 
  public static void displaySVMCModelDetails(
    Model model) throws JDMException
  {
    // Obtains model details
    SVMClassificationModelDetail svmcModelDetails = 
      (SVMClassificationModelDetail)model.getModelDetail();
    // Is linear model? 
    System.out.println("Is linear model? " + svmcModelDetails.isLinearSVMModel() );
    // Available targets
    CategorySet targetSet = ((OraClassificationModel)model).getTargetCategorySet();
    Object[] targets = targetSet.getValues();
    // Attribute coefficient value will be available if it is a linear model
    if (svmcModelDetails.isLinearSVMModel())
    {
      System.out.println("Model Deatils: ( Target Value, Attribute Name, Attribute Value, Coefficient)");
      MessageFormat mfDetails = new MessageFormat("               ( {0}, {1}, {2}, {3} )");
      String[] vals = new String[4]; 
      for (int row =0; row < targets.length; row++)
      {
        // Print bias of the current target value by invoking getBias method.
        vals[0] =(String) targets[row].toString();
        vals[1] = "";
        vals[2] = "";
        vals[3] = m_df.format(svmcModelDetails.getBias(targets[row])) + " (Bias)"; 
        System.out.println( mfDetails.format(vals) );
        //Get the signature attributes
        ModelSignature modelSignature = model.getSignature();      
        Collection sortedSet = modelSignature.getAttributes();
        Iterator attrIterator = sortedSet.iterator();
        
        while(attrIterator.hasNext()) 
        {
          SignatureAttribute attr = (SignatureAttribute)attrIterator.next();
          // Print all attribute coefficients -- The FIRST row in the SVM model 
          // details output shows the value for SVM bias under the COEFFICIENT 
          // column.
          Map attrCoefficientMap = ((OraSVMClassificationModelDetail)svmcModelDetails).getCoefficients(targets[row],attr.getName());
          Object[] attrVals = attrCoefficientMap.keySet().toArray();
          if(attrVals != null) {
            for(int iAttr=0; iAttr < attrVals.length; iAttr++) 
            {            
              vals[1] = attr.getName();
              vals[2] = "";
              if(attrVals[iAttr] != null)
                vals[2] = attrVals[iAttr].toString();
              vals[3] = "";
                Number coefficient = (Number)attrCoefficientMap.get(attrVals[iAttr]);
                if(coefficient != null)
                  vals[3] = m_df.format(coefficient);
              System.out.println( mfDetails.format(vals) );
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
    // Retrieve Oracle SVMC model test metrics deatils extensions
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
    System.out.println("Confusion Matrix:( Actual, Prection, Value )");
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
      // Build table header
      for(int iCol=1; iCol<=colCount; iCol++) 
      {
        String colName = rsMeta.getColumnName(iCol);
        header.append(emptyCol.replace(0, colName.length(), colName));
        emptyCol = new StringBuffer("                    ");
      }
      System.out.println(header.toString());
      // Write table data
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
    } // Ignore
  }
  
  private static void clean() 
  {
    //Drop the model
    try {
      m_dmeConn.removeObject("svmcModel_jdm", NamedObject.model);
    } catch(JDMException jdmExp) {}
    
  }
}

