// Copyright (c) 2004, 2006, Oracle. All rights reserved.  
// File: dmtxtsvmdemo.java
/**
*   This demo program describes how to use the Oracle Data Mining (ODM) Java API
* to solve a classification problem using the Support Vector Machines (SVM) 
* algorithm.
* ------------------------------------------------------------------------------
*                             PROBLEM DEFINITION
* ------------------------------------------------------------------------------
*   How to predict whether a customer responds or not to the new affinity card 
* program using a classifier based on the SVM algorithm with text mining?
* ------------------------------------------------------------------------------
*                             DATA DESCRIPTION
* ------------------------------------------------------------------------------
*   Data for this demo is composed from base tables in Sales History (SH) 
* Schema. The SH schema is an Oracle Database Sample Schema that has the customer
* comments(text), demographics, purchasing, and response details for the 
* previous affinity card programs. Data exploration and preparing the data is a 
* common step before doing data mining. Here in this demo, the following views are 
* created in the user schema using CUSTOMERS, COUNTRIES, and 
* SUPPLIMENTARY_DEMOGRAPHICS tables.
* 
* MINING_BUILD_TEXT:
*   This view collects the previous customers' comments(text), demographics, 
*   purchasing, and affinity card response details for building the model.
* 
* MINING_APPLY_TEXT:
*   This view collects the prospective customers' comments(text), demographics, 
*   and purchasing details for predicting response for the new affinity card 
*   program.
* ------------------------------------------------------------------------------
*                             DATA MINING PROCESS
* ------------------------------------------------------------------------------
*   Prepare Data:
*   1. Missing Value treatment for Predictors in Build, Test and Apply data
*   See dmsvcdemo.java for a definition of missing values, and the steps to be 
*   taken for missing value imputation.  SVM interprets all NULL values for a 
*   given attribute as "sparse".  We skip missing values treatment in this demo.
*
*   2. Outlier/Clipping Treatment for Predictors for Build data
*   See dmsvcdemo.java for discussion of outlier treatment.  We skip outlier 
*   treatment in this demo.
*
*   3. Normalize Predictors in Build, Test, and Apply data, unlike Regression 
*   using SVM, the target attribute is NOT normalized.
*   
*   The PrepareData() method in this demo program illustrates the preparation of the 
*   build and apply data.
*         
*   Build Model:
*     Mining Model is the prime object in data mining. The buildModel() method
*     illustrates how to build a classification model using the SVM algorithm with 
*     text mining.
*     
*   Apply Model:
*     Predicting the target attribute values is the prime function of 
*     classification text mining models. The applyModel() method illustrates how to 
*     predict the customer response for an affinity card program.
* ------------------------------------------------------------------------------
*                             EXECUTING DEMO PROGRAM
* ------------------------------------------------------------------------------
*   Refer to Oracle Data Mining Administrator's Guide
*   for guidelines for executing this demo program.
*/
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
import java.util.Iterator;
import java.util.Map;
// Java Data Mining (JDM) standard api imports
import javax.datamining.ExecutionHandle;
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
import javax.datamining.supervised.classification.ClassificationTestMetricsTaskFactory;
import javax.datamining.supervised.classification.ClassificationTestTaskFactory;
import javax.datamining.task.BuildTask;
import javax.datamining.task.BuildTaskFactory;
import javax.datamining.task.apply.DataSetApplyTask;
import javax.datamining.task.apply.DataSetApplyTaskFactory;
// Oracle Java Data Mining (JDM) implemented api imports
import oracle.dmt.jdm.algorithm.svm.classification.OraSVMClassificationSettings;
import oracle.dmt.jdm.modeldetail.svm.OraSVMClassificationModelDetail;
import oracle.dmt.jdm.resource.OraConnection;
import oracle.dmt.jdm.resource.OraConnectionFactory;
import oracle.dmt.jdm.supervised.classification.OraClassificationModel;
import oracle.dmt.jdm.task.OraPredictTaskFactory;
import oracle.dmt.jdm.task.OraTransformationTask;
import oracle.dmt.jdm.task.OraTransformationTaskFactory;
import oracle.dmt.jdm.transform.clipping.OraClippingTransformFactory;
import oracle.dmt.jdm.transform.normalize.OraNormalizeTransformFactory;
import oracle.dmt.jdm.transform.normalize.OraNormalizeTransform;
import oracle.dmt.jdm.transform.normalize.OraNormalizeType;
import oracle.dmt.jdm.transform.text.OraTextTransform;
import oracle.dmt.jdm.transform.text.OraTextTransformFactory;


public class dmtxtsvmdemo extends Object {
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
  private static OraPredictTaskFactory m_predictFactory;
  private static ClassificationApplySettingsFactory m_applySettingsFactory;
  private static ClassificationTestMetricsTaskFactory m_testMetricsTaskFactory;
  private static OraTextTransformFactory m_textXformFactory;
  private static OraNormalizeTransformFactory m_normalizeXformFactory;
  private static OraClippingTransformFactory m_clippingXformFactory;
  private static OraTransformationTaskFactory m_xformTaskFactory;
  // Global constants 
  private static DecimalFormat m_df = new DecimalFormat("##.####");
  // Global data members 
  private static OraNormalizeTransform m_normalizeDataXform;
  
  private static boolean displayModelDeatils = false;//By default it is set to false. To view the details of the model this flag can be enabled
  
  public static void main( String args[] ) { 
    try { 
        if (( args.length != 0 ) & ( args.length != 3 )) {
          System.out.println("Usage: java dmtxtsvmdemo ");
          System.out.println("   or: java dmtxtsvmdemo <Host name>:<Port>:<SID> <User Name> <Password>");
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
        // 4. Prepare data
        prepareData();
        // 5. Build model
        buildModel();
        // 6. Test model - See dmsvcdemo.java for examples. The key difference 
        // here is the nested table input, and you can adapt that sample code to 
        // accept the table with a nested table column as input.
        // 7. Apply the model
        applyModel();
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
    m_predictFactory = (OraPredictTaskFactory)m_dmeConn.getFactory(
      "oracle.dmt.jdm.task.OraPredictTask");      
    m_testFactory = (ClassificationTestTaskFactory)m_dmeConn.getFactory(
      "javax.datamining.supervised.classification.ClassificationTestTask");
    m_dsApplyFactory = (DataSetApplyTaskFactory)m_dmeConn.getFactory(
      "javax.datamining.task.apply.DataSetApplyTask");
    m_applySettingsFactory = (ClassificationApplySettingsFactory)m_dmeConn.getFactory(
      "javax.datamining.supervised.classification.ClassificationApplySettings");
    m_testMetricsTaskFactory = (ClassificationTestMetricsTaskFactory) m_dmeConn.getFactory(
      "javax.datamining.supervised.classification.ClassificationTestMetricsTask");
    m_clippingXformFactory = (OraClippingTransformFactory)m_dmeConn.getFactory(
      "oracle.dmt.jdm.transform.clipping.OraClippingTransform");
    m_normalizeXformFactory = (OraNormalizeTransformFactory)m_dmeConn.getFactory(
      "oracle.dmt.jdm.transform.normalize.OraNormalizeTransform");
    m_textXformFactory = (OraTextTransformFactory)m_dmeConn.getFactory(
      "oracle.dmt.jdm.transform.text.OraTextTransform");
    m_xformTaskFactory = (OraTransformationTaskFactory)m_dmeConn.getFactory(
      "oracle.dmt.jdm.task.OraTransformationTask");
  }

  /**
   *   This method illustrates preparation of the data for build, test, and apply 
   * operations by using missing value, clipping and normalization 
   * transformation.  
   * 
   *   The following table illustrates customer attributes available for building
   * the mining model and data characteristics and type of transformation used
   * for each attribute.
   * 
   * Transformations:
   *   1. Missing Value (MISSING): Skipped - see dmsvcdemo.java for example.
   *      
   *   2. Clipping (CLIPPING): Skipped - see dmsvcdemo.java for example.
   *   
   *   3. Normalization (NORMALIZING): Normalize numerical attributes AGE and 
   *      YRS_RESIDENCE.  We will use default(minmax) normalization type to 
   *      treat these numerical attributes.
   * 
   *   COLUMN_NAME              DATA_TYPE                  DISTINCT NORMALIZING
   *   ------------------       ---------                  -------- -----------
   *   CUST_ID                  NUMBER                     1500     
   *   CUST_GENDER              CHAR                       2            
   *   AGE                      NUMBER                     66       MINMAX 
   *   CUST_MARITAL_STATUS      VARCHAR2                   7        
   *   COUNTRY_NAME             VARCHAR2                   19                      
   *   CUST_INCOME_LEVEL        VARCHAR2                   12       
   *   EDUCATION                VARCHAR2                   16       
   *   OCCUPATION               VARCHAR2                   15                       
   *   HOUSEHOLD_SIZE           VARCHAR2                   6        
   *   YRS_RESIDENCE            NUMBER                     15       MINMAX        
   *   AFFINITY_CARD            NUMBER                     2        
   *   BULK_PACK_DISKETTES      NUMBER                     2        
   *   FLAT_PANEL_MONITOR       NUMBER                     2        
   *   HOME_THEATER_PACKAGE     NUMBER                     2             
   *   BOOKKEEPING_APPLICATION  NUMBER                     2         
   *   PRINTER_SUPPLIES         NUMBER                     1         
   *   Y_BOX_GAMES              NUMBER                     2         
   *   OS_DOC_SET_KANJI         NUMBER                     2         
   *   OS_DOC_SET_KANJI         NUMBER                     2         
   *   COMMENTS                 VARCHAR2(4000)             44
   *   
   *   The following tables are created after the execution of this method. 
   * 
   *   Unprepared Data           --->  Prepared(Normalized) Data
   *   ------------------------        ---------------------------
   *   MINING_BUILD_TEXT               TXTSVM_NORM_DATA_BUILD_JDM
   *   MINING_APPLY_TEXT               TXTSVM_NORM_DATA_APPLY_JDM
   */
  public static void prepareData() throws JDMException 
  {
      boolean isOutputAsView = false;
      String inputDataURI = null;
      String outputDataURI = null; 
      String inputNormalizationDefinitionTable = null;
      String inputClippingDefinitionTable = null;
      OraTransformationTask xformTask = null;
      //------------------------------------------------------------------------
      // 1. Prepare build data
      //------------------------------------------------------------------------        
      // Text Transformation - Convert text column into a nested column in order
      // to perform text mining
      isOutputAsView = false;
      inputDataURI = "MINING_BUILD_TEXT";
      outputDataURI = "TXTSVM_BUILD_NESTED_TEXT";
      String caseId = "CUST_ID";
      String[] textColumnList = {"COMMENTS"};  
      OraTextTransform textDataXform = 
          (OraTextTransform)m_textXformFactory.create(
                           inputDataURI, outputDataURI, caseId, textColumnList);
      xformTask = m_xformTaskFactory.create(textDataXform);
      executeTask(xformTask, "txtsvmPrepareTextB_jdm"); 
      // Normalization - unlike SVM regression, the classification target is 
      // NOT normalized here.  In addition, case id and binary numerical 
      // attributes do not need to be normalized in any case.
      //
      // Create normalization definition table from mining build nested data
      isOutputAsView = true;
      inputDataURI = "TXTSVM_BUILD_NESTED_TEXT";
      outputDataURI = "TXTSVM_NORM_DATA_BUILD_JDM";
      OraNormalizeType normalizeType = OraNormalizeType.min_max;
      Integer roundingNumber = new Integer(0);
      OraNormalizeTransform buildDataXform = 
          (OraNormalizeTransform)m_normalizeXformFactory.create(
                              inputDataURI, outputDataURI, isOutputAsView,
                              normalizeType, roundingNumber);
      String[] excludeColumnList = {
                          "CUST_ID",
                          "AFFINITY_CARD",
                          "BULK_PACK_DISKETTES",
                          "FLAT_PANEL_MONITOR",
                          "HOME_THEATER_PACKAGE",
                          "BOOKKEEPING_APPLICATION",
                          "PRINTER_SUPPLIES",
                          "Y_BOX_GAMES",
                          "OS_DOC_SET_KANJI",
                          "COMMENTS"
                      };  
      buildDataXform.setExcludeColumnList(excludeColumnList);
      xformTask = m_xformTaskFactory.create(buildDataXform);
      executeTask(xformTask, "txtsvmPrepareBuild_jdm"); 
      //------------------------------------------------------------------------
      // 2. Prepare apply data - if the data for model creation has been 
      //    prepared, then the data to be scored using the model must be 
      //    prepared in the same manner in order to obtain meaningful results.
      //------------------------------------------------------------------------
      // Text Transformation - Convert text column into a nested column in order
      // to perform text mining
      isOutputAsView = false;
      inputDataURI = "MINING_APPLY_TEXT";
      outputDataURI = "TXTSVM_APPLY_NESTED_TEXT";
      caseId = "CUST_ID";
      textColumnList = new String[] {"COMMENTS"}; 
      //Get feature tables created by the build text transformation
      OraTransformationTask buildTextXformTask = (OraTransformationTask)m_dmeConn.retrieveObject(
        "txtsvmPrepareTextB_jdm", NamedObject.task);
      String[] buildFeatureTables = ((OraTextTransform)buildTextXformTask.getTransformation()).getFeatureTables();
      textDataXform = 
          (OraTextTransform)m_textXformFactory.create(
                           inputDataURI, outputDataURI, caseId, textColumnList, 
                            buildFeatureTables);
      xformTask = m_xformTaskFactory.create(textDataXform);
      executeTask(xformTask, "txtsvmPrepareTextA_jdm"); 
      // Normalization      
      isOutputAsView = true;
      inputDataURI = "TXTSVM_APPLY_NESTED_TEXT";
      outputDataURI = "TXTSVM_NORM_DATA_APPLY_JDM";
      inputNormalizationDefinitionTable = 
                              buildDataXform.getNormalizationDefinitionTable();
      OraNormalizeTransform applyDataXform = 
          (OraNormalizeTransform)m_normalizeXformFactory.create(
                            inputDataURI, outputDataURI, 
                            isOutputAsView, inputNormalizationDefinitionTable);
      xformTask = m_xformTaskFactory.create(applyDataXform);
      executeTask(xformTask, "txtsvmPrepareApply_jdm");  
  }

  /**
   *   This method illustrates how to build a text mining model using the
   * TXTSVM_NORM_DATA_BUILD_JDM dataset and classification settings with 
   * the SVM algorithm. 
   * 
   *    By default, the SVM algorithm chooses a kernel type automatically. This 
   * choice can be overriden by the user. Linear kernel is preferred for high 
   * dimensional data, and Gaussian kernel for low dimensional data. Here we use 
   * linear kernel to demonstrate the getModelDetail() API, which applies only 
   * for models.  
   */
  public static void buildModel() throws JDMException 
  {
      System.out.println("---------------------------------------------------");    
      System.out.println("--- Build Model                                 ---");
      System.out.println("---------------------------------------------------");
     // 1. Create & save PhysicalDataSpecification 
      PhysicalDataSet buildData = 
                        m_pdsFactory.create("TXTSVM_NORM_DATA_BUILD_JDM", false);
      PhysicalAttribute pa = m_paFactory.create("CUST_ID", 
        AttributeDataType.integerType, PhysicalAttributeRole.caseId );
      buildData.addAttribute(pa);
      m_dmeConn.saveObject("txtsvmBuildData_jdm", buildData, true);
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
      m_dmeConn.saveObject("txtsvmBuildSettings_jdm", buildSettings, true);
     // 3. Create, save & execute Build Task      
      BuildTask buildTask = m_buildFactory.create(
                     "txtsvmBuildData_jdm", //Build data specification
                     "txtsvmBuildSettings_jdm", //Mining function settings name
                     "txtsvmModel_jdm" //Mining model name
                     );                          
      buildTask.setDescription("txtsvmBuildTask_jdm");
      executeTask(buildTask, "txtsvmBuildTask_jdm");
     //4. Restore the model from the DME and explore the details of the model
      ClassificationModel model = 
        (ClassificationModel)m_dmeConn.retrieveObject(
          "txtsvmModel_jdm", NamedObject.model);
      // Display model build settings
      ClassificationSettings retrievedBuildSettings = 
                               (ClassificationSettings)model.getBuildSettings();
      if(buildSettings == null) 
          System.out.println("Failure to restore build settings.");
      else 
          displayBuildSettings(retrievedBuildSettings, "txtsvmBuildSettings_jdm");
      // Display model signature    
      displayModelSignature((Model)model);
      // Display model details
      if(displayModelDeatils)
       displaySVMCModelDetails((Model)model);
  }
  
  /**
   *   This method illustrates how to apply the mining model on the
   * TXTSVM_NORM_DATA_APPLY_JDM dataset to predict customer
   * response. After completion of the task, an apply output table with the
   * predicted results is created at the user-specified location. 
   */ 
  public static void applyModel() throws JDMException
  {
      System.out.println("---------------------------------------------------");    
      System.out.println("--- Apply Model                                 ---");
      System.out.println("---------------------------------------------------");
      // The key here is to demonstrate the use of these functions on a scoring 
      // table with nested table input for attributes.
    // 1. Create & save PhysicalDataSpecification
      PhysicalDataSet applyData = m_pdsFactory.create(
                                            "TXTSVM_NORM_DATA_APPLY_JDM", false );
      PhysicalAttribute pa = m_paFactory.create("CUST_ID", 
        AttributeDataType.integerType, PhysicalAttributeRole.caseId );
      applyData.addAttribute( pa );
      m_dmeConn.saveObject( "txtsvmApplyData_jdm", applyData, true );
    // 2. Create & save ClassificationApplySettings
      ClassificationApplySettings clasAS = m_applySettingsFactory.create();
      m_dmeConn.saveObject( "txtsvmApplySettings_jdm", clasAS, true);           
    // 3. Create, store & execute apply Task
     DataSetApplyTask applyTask = m_dsApplyFactory.create(
                              "txtsvmApplyData_jdm", "txtsvmModel_jdm", 
                              "txtsvmApplySettings_jdm", "TXTSVM_APPLY_OUTPUT_JDM");
     executeTask(applyTask, "txtsvmApplyTask_jdm");
     // 4. Display apply result -- Note that APPLY results do not need to be 
     //    reverse transformed, as done in the case of model details. This is 
     //    because class values of a classification target were not (required to 
     //    be) binned or normalized.
     //
     // Find the 10 customers who are most likely to use an affinity card.
     // Note that the SQL data mining functions seamless work against
     // tables that contain nested table columns of type DM_Nested_Numerical
     // or DM_Nested_Categorical. The nested column COMMENT is also part
     // of this input.
     displayTable("TXTSVM_APPLY_OUTPUT_JDM", 
                  "where PREDICTION =1 and ROWNUM < 11", 
                  "order by PROBABILITY desc");
  }  
  
  /**
   * This method stores the given task with the specified name in the DMS 
   * and submits the task for asynchronous execution in the DMS. After 
   * completing the task successfully it returns true. If there is a task 
   * failure, then it prints error description and returns false.
   * 
   * @param taskObj task object
   * @param taskName name of the task
   * 
   * @return boolean returns true when the task is successful
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
    java.sql.Connection dbConn = 
      ((OraConnection)m_dmeConn).getDatabaseConnection();
    Statement stmt = null;
    //Drop prepared tables
    try 
    {
      stmt = dbConn.createStatement();
      stmt.executeUpdate("DROP TABLE TXTSVM_BUILD_NESTED_TEXT");
    } catch(SQLException anySqlExp) {} // Ignore
    finally{
      try {
        stmt.close();
      }
      catch(Exception anySqlExp){}
    } 
    try 
    {
      stmt = dbConn.createStatement();
      stmt.executeUpdate("DROP TABLE TXTSVM_APPLY_NESTED_TEXT");
    } catch(SQLException anySqlExp) {} // Ignore
    finally{
      try {
        stmt.close();
      }
      catch(Exception anySqlExp){}
    } 
    //Drop prepared views
    try 
    {
      stmt = dbConn.createStatement();
      stmt.executeUpdate("DROP VIEW TXTSVM_NORM_DATA_BUILD_JDM");
    } catch(SQLException anySqlExp) {} // Ignore
    finally{
      try {
        stmt.close();
      }
      catch(Exception anySqlExp){}
    }
    try 
    {
      stmt = dbConn.createStatement();
      stmt.executeUpdate("DROP VIEW TXTSVM_NORM_DATA_APPLY_JDM");
    } catch(SQLException anySqlExp) {} // Ignore    
    finally{
      try {
        stmt.close();
      }
      catch(Exception anySqlExp){}
    }
    //Drop apply output table
    try 
    {
      stmt = dbConn.createStatement();
      stmt.executeUpdate("DROP TABLE TXTSVM_APPLY_OUTPUT_JDM");
    } catch(SQLException anySqlExp) {} // Ignore
    finally{
      try {
        stmt.close();
      }
      catch(Exception anySqlExp){}
    }
    //Drop the model
    try {
      m_dmeConn.removeObject("txtsvmModel_jdm", NamedObject.model);
    } catch(JDMException jdmExp) {}
  }
}
