// Copyright (c) 2004, 2006, Oracle. All rights reserved.  
// File: dmtxtnmfdemo.java

/**
* This demo program describes how to use the Oracle Data Mining (ODM) Java API 
* to build and apply a feature extraction model using the non-negative matrix 
* factorization (NMF) algorithm.
* ------------------------------------------------------------------------------
*                             PROBLEM DEFINITION
* ------------------------------------------------------------------------------
*   Given demographic data about a set of customers, extract features
* from the given dataset with text mining.
* ------------------------------------------------------------------------------
*                             DATA DESCRIPTION
* ------------------------------------------------------------------------------
*   Data for this demo is composed from base tables in the Sales History (SH) 
* Schema. The SH schema is an Oracle Database Sample Schema that has the customer
* demographics, purchasing, and response details for the previous affinity card 
* programs. Data exploration and preparing the data is a common step before 
* doing data mining. Here in this demo, the following views are created 
* in the user schema using CUSTOMERS, COUNTRIES, and SUPPLIMENTARY_DEMOGRAPHICS 
* tables.
* 
* MINING_BUILD_TEXT:
*   This view collects the previous customers' comments(text), demographics,  
*   purchasing details, and affinity card response details for building the model.
* 
* MINING_APPLY_TEXT:
*   This view collects the prospective customers' comments(text), demographics,
*   and purchasing details for predicting response to the new affinity card 
*   program.
* ------------------------------------------------------------------------------
*                             DATA MINING PROCESS
* ------------------------------------------------------------------------------
*   Prepare Data:
*     In general, attributes need to be of similar scale in order to be treated
*     equally during the model build. Therefore normalization is required.
*     The prepareData() method illustrates the normalization of the build and apply 
*     data.
*   Build Model:
*     Mining Model is the prime object in data mining. The buildModel() method
*     illustrates how to build a feature extraction model using the NMF algorithm.
*   Apply Model:
*     For a descriptive mining function like feature extraction, "scoring"
*     involves providing the probability values for each feature.
*     During model apply, an NMF model maps the original data into the 
*     new set of attributes (features) discovered by the model.
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
import java.sql.SQLException;
import java.sql.Statement;
import java.text.DecimalFormat;
import java.text.MessageFormat;
import java.util.Collection;
import java.util.Iterator;
// Java Data Mining (JDM) standard imports
import java.util.Map;
import java.util.TreeMap;
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
import javax.datamining.resource.Connection;
import javax.datamining.resource.ConnectionFactory;
import javax.datamining.resource.ConnectionSpec;
import javax.datamining.task.BuildTask;
import javax.datamining.task.BuildTaskFactory;
import javax.datamining.task.apply.DataSetApplyTask;
import javax.datamining.task.apply.DataSetApplyTaskFactory;
// Oracle Java Data Mining (JDM) implemented api imports
import oracle.dmt.jdm.algorithm.nmf.OraNMFAlgorithmSettingsFactory;
import oracle.dmt.jdm.algorithm.nmf.OraNMFAlgorithmSettings;
import oracle.dmt.jdm.featureextraction.OraFeature;
import oracle.dmt.jdm.featureextraction.OraFeatureExtractionApplySettings;
import oracle.dmt.jdm.featureextraction.OraFeatureExtractionApplySettingsFactory;
import oracle.dmt.jdm.featureextraction.OraFeatureExtractionModel;
import oracle.dmt.jdm.featureextraction.OraFeatureExtractionSettings;
import oracle.dmt.jdm.featureextraction.OraFeatureExtractionSettingsFactory;
import oracle.dmt.jdm.resource.OraConnection;
import oracle.dmt.jdm.resource.OraConnectionFactory;
import oracle.dmt.jdm.task.OraTransformationTask;
import oracle.dmt.jdm.task.OraTransformationTaskFactory;
import oracle.dmt.jdm.transform.normalize.OraNormalizeTransformFactory;
import oracle.dmt.jdm.transform.normalize.OraNormalizeTransform;
import oracle.dmt.jdm.transform.normalize.OraNormalizeType;
import oracle.dmt.jdm.transform.text.OraTextTransform;
import oracle.dmt.jdm.transform.text.OraTextTransformFactory;


public class dmtxtnmfdemo extends Object{
  //Connection related data members
  private static Connection m_dmeConn;
  private static ConnectionFactory m_dmeConnFactory;
  //Object factories used in this demo program
  private static PhysicalDataSetFactory m_pdsFactory;
  private static PhysicalAttributeFactory m_paFactory;
  private static OraFeatureExtractionSettingsFactory m_feSettingFactory;
  private static OraNMFAlgorithmSettingsFactory m_feAlgFactory;
  private static BuildTaskFactory m_buildFactory;
  private static DataSetApplyTaskFactory m_dsApplyFactory;
  private static OraFeatureExtractionApplySettingsFactory m_applySettingsFactory;
  private static OraTextTransformFactory m_textXformFactory;
  private static OraNormalizeTransformFactory m_normalizeXformFactory;
  private static OraTransformationTaskFactory m_xformTaskFactory;
  // Global constants used for formatting output
  private static String UNDERLINE  = "*************************************";
  private static String TAB  = "    ";
  private static char SPACE = ' ';
  private static int SECOND_COLUMN = 40;
  private static int THIRD_COLUMN  = 70;
  // Global constants 
  private static DecimalFormat m_df = new DecimalFormat("##.####");
  private static boolean displayModelDeatils = false;//By default it is set to false. To view the details of the model this flag can be enabled
  
  public static void main( String args[] ) { 
    try {      
        if (( args.length != 0 ) & ( args.length != 3 )) {
          System.out.println("Usage: java dmtxtnmfdemo ");
          System.out.println("   or: java dmtxtnmfdemo <Host name>:<Port>:<SID> <User Name> <Password>");
          return;
        }
        String uri = args[0];
        String name =  args[1];
        String password = args[2]; 
        //1. Login to the Data Mining Engine
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
        // 5. Build a model
        buildModel();
        // 6. Apply the model
        applyModel();
    } catch(Exception anyExp) {
      anyExp.printStackTrace(System.out);
    } finally {
      try {
        // 7. Logout from the Data Mining Engine
        m_dmeConn.close();
      } catch(Exception anyExp1) { }//Ignore
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
    m_feSettingFactory = (OraFeatureExtractionSettingsFactory)m_dmeConn.getFactory(
      "oracle.dmt.jdm.featureextraction.OraFeatureExtractionSettings");
    m_feAlgFactory = (OraNMFAlgorithmSettingsFactory)m_dmeConn.getFactory(
      "oracle.dmt.jdm.algorithm.nmf.OraNMFAlgorithmSettings");
    m_buildFactory = (BuildTaskFactory)m_dmeConn.getFactory(
      "javax.datamining.task.BuildTask");
    m_dsApplyFactory = (DataSetApplyTaskFactory)m_dmeConn.getFactory(
      "javax.datamining.task.apply.DataSetApplyTask");
    m_applySettingsFactory = (OraFeatureExtractionApplySettingsFactory)m_dmeConn.getFactory(
      "oracle.dmt.jdm.featureextraction.OraFeatureExtractionApplySettings");
    m_normalizeXformFactory = (OraNormalizeTransformFactory)m_dmeConn.getFactory(
      "oracle.dmt.jdm.transform.normalize.OraNormalizeTransform");
    m_textXformFactory = (OraTextTransformFactory)m_dmeConn.getFactory(
      "oracle.dmt.jdm.transform.text.OraTextTransform");
    m_xformTaskFactory = (OraTransformationTaskFactory)m_dmeConn.getFactory(
      "oracle.dmt.jdm.task.OraTransformationTask");
  }

  /**
   *   This method illustrates preparation of the data for the build and apply 
   * operations by using normalization transformation. 
   * 
   *   Here numerical attribute AGE is prepared using default normalization
   * which is min-max normalization. The values for shift and scale are computed 
   * to be shift = min, and scale = (max - min) respectively. 
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
   *   CPMMENTS                 VARCHAR2(4000)             44          
   *         
   *   The following views are created after the execution of this method. 
   * 
   *   Unprepared Data           --->  Prepared(Normalized) Data
   *   ------------------------        ---------------------------
   *   MINING_BUILD_TEXT               TXTNMF_NORM_DATA_BUILD_JDM
   *   MINING_APPLY_TEXT               TXTNMF_NORM_DATA_APPLY_JDM
   *
   * @exception JDMException if data preparation failed
   */
  public static void prepareData() throws JDMException 
  {
      boolean isOutputAsView = false;
      String inputDataURI = null;
      String outputDataURI = null; 
      String inputNormalizationDefinitionTable = null;
      OraTransformationTask xformTask = null;
      //------------------------------------------------------------------------
      // 1. Prepare build data
      //------------------------------------------------------------------------        
      // Text Transformation - Convert text column into a nested column in order
      // to perform text mining
      isOutputAsView = false;
      inputDataURI = "MINING_BUILD_TEXT";
      outputDataURI = "TXTNMF_BUILD_NESTED_TEXT";
      String caseId = "CUST_ID";
      String[] textColumnList = {"COMMENTS"};  
      OraTextTransform textDataXform = 
          (OraTextTransform)m_textXformFactory.create(
                           inputDataURI, outputDataURI, caseId, textColumnList);
      xformTask = m_xformTaskFactory.create(textDataXform);
      executeTask(xformTask, "txtnmfPrepareTextB_jdm"); 
      // Normalization
      isOutputAsView = true;
      inputDataURI = "TXTNMF_BUILD_NESTED_TEXT";
      outputDataURI = "TXTNMF_NORM_DATA_BUILD_JDM";
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
      executeTask(xformTask, "txtnmfPrepareBuild_jdm");    
      //------------------------------------------------------------------------
      // 2. Prepare apply data
      //------------------------------------------------------------------------        
      // Text Transformation - Convert text column into a nested column in order
      // to perform text mining
      isOutputAsView = false;
      inputDataURI = "MINING_APPLY_TEXT";
      outputDataURI = "TXTNMF_APPLY_NESTED_TEXT";
      caseId = "CUST_ID";
      textColumnList = new String[] {"COMMENTS"};  
      //Get feature tables created by the build text transformation
      OraTransformationTask buildTextXformTask = (OraTransformationTask)m_dmeConn.retrieveObject(
        "txtnmfPrepareTextB_jdm", NamedObject.task);
      String[] buildFeatureTables = ((OraTextTransform)buildTextXformTask.getTransformation()).getFeatureTables();
      textDataXform = 
          (OraTextTransform)m_textXformFactory.create(
                           inputDataURI, outputDataURI, caseId, textColumnList,
                           buildFeatureTables);
      xformTask = m_xformTaskFactory.create(textDataXform);
      executeTask(xformTask, "txtnmfPrepareTextA_jdm"); 
      // Normalization
      isOutputAsView = true;
      inputDataURI = "TXTNMF_APPLY_NESTED_TEXT";
      outputDataURI = "TXTNMF_NORM_DATA_APPLY_JDM";
      inputNormalizationDefinitionTable = buildDataXform.getNormalizationDefinitionTable();
      OraNormalizeTransform applyDataXform = 
          (OraNormalizeTransform)m_normalizeXformFactory.create(
                              inputDataURI, outputDataURI, isOutputAsView, inputNormalizationDefinitionTable);
      xformTask = m_xformTaskFactory.create(applyDataXform);
      executeTask(xformTask, "txtnmfPrepareApply_jdm");   
  }
  
  /**
   *   This method illustrates how to build a mining model using the
   * "TXTNMF_NORM_DATA_BUILD_JDM" dataset with the NMF algorithm.
   * 
   *   After completing the build task, the model named "txtnmfModel_jdm" will 
   * be created.
   * 
   * @exception JDMException if model build failed
   */
  public static void buildModel() throws JDMException 
  {
    // 1. Create & save PhysicalDataSpecification      
    PhysicalDataSet buildData = m_pdsFactory.create("TXTNMF_NORM_DATA_BUILD_JDM", false);
    PhysicalAttribute pa = m_paFactory.create("CUST_ID", 
      AttributeDataType.integerType, PhysicalAttributeRole.caseId );
    buildData.addAttribute(pa);
    m_dmeConn.saveObject("txtnmfBuildData_jdm", buildData, true);
    // 2. Create & save Mining Function Settings
    //Create NMF algorithm settings
    OraNMFAlgorithmSettings nmfAlgo = 
                          (OraNMFAlgorithmSettings)m_feAlgFactory.create();
    // Examples settings are:
    // nmfAlgo.setMaxNumberOfIterations(10);
    // nmfAlgo.setMinConvergenceTolerance(0.05);
    // nmfAlgo.setSeedValue(-1);
    //
    // Create OraFeatureExtractionSettings
    OraFeatureExtractionSettings buildSettings = m_feSettingFactory.create();
    buildSettings.setAlgorithmSettings(nmfAlgo);
    buildSettings.setNumberOfFeatures(10);
    m_dmeConn.saveObject("txtnmfBuildSettings_jdm", buildSettings, true);
    // 3. Create, save & execute Build Task      
    BuildTask buildTask = m_buildFactory.create(
                   "txtnmfBuildData_jdm", //Build data specification
                   "txtnmfBuildSettings_jdm", //Mining function settings name
                   "txtnmfModel_jdm" //Mining model name
                   );                          
    buildTask.setDescription("txtnmfBuildTask_jdm");
    executeTask(buildTask, "txtnmfBuildTask_jdm"); 
    // 4. Restore the model from the DME and explore the details of the model
    OraFeatureExtractionModel model = (OraFeatureExtractionModel)
      m_dmeConn.retrieveObject("txtnmfModel_jdm", NamedObject.model);
      
    // Display model build settings
    OraFeatureExtractionSettings retrievedBuildSettings = 
                             (OraFeatureExtractionSettings)model.getBuildSettings();
    if(buildSettings == null) 
        System.out.println("Failure to restore build settings.");
    else 
        displayBuildSettings(retrievedBuildSettings, "txtnmfBuildSettings_jdm");
    // Display model signature    
    displayModelSignature((Model)model);
    // Display model details       
    if(displayModelDeatils)
      displayFEModelDetails(model);
  }        

  /**
   * The Apply operation is performed by a task that requires the location of
   * the dataset, the model name, the output specification, and the location
   * of the output.
   * 
   * The output table "nmf_apply_output_jdm" is specified by a MiningApplyOutput
   * object to contain the feature identifier and the match quality for feature
   * extraction models.
   * 
   * In this example, the NMF model is applied with the 
   * "TXTNMF_NORM_DATA_APPLY_JDM" prepared dataset to obtain the feature 
   * identifier and the match quality.
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
        "TXTNMF_NORM_DATA_APPLY_JDM", false );
      PhysicalAttribute pa = m_paFactory.create("CUST_ID", 
        AttributeDataType.integerType, PhysicalAttributeRole.caseId );
      applyData.addAttribute( pa );
      m_dmeConn.saveObject( "txtnmfNMFData_jdm", applyData, true );
    // 2. Create & save FeatureExtractionApplySettings
      OraFeatureExtractionApplySettings feAS = m_applySettingsFactory.create();
      m_dmeConn.saveObject( "txtnmfApplySettings_jdm", feAS, true);
            
    // 3. Create, store & execute apply Task
     DataSetApplyTask applyTask = m_dsApplyFactory.create(
        "txtnmfNMFData_jdm", "txtnmfModel_jdm", "txtnmfApplySettings_jdm", 
        "TXTNMF_APPLY_OUTPUT_JDM");
     executeTask(applyTask, "txtnmfApplyTask_jdm");
     // 4. Display apply result -- the first 10 rows
     displayTable("TXTNMF_APPLY_OUTPUT_JDM", 
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
  public static boolean executeTask(Task taskObj, String taskName) throws JDMException 
  {
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
    OraFeatureExtractionSettings featureSettings, String buildSettingsName) 
  {
    // Display build settings table
    System.out.println("BuildSettings Details from the " 
      + buildSettingsName + " table:");
      displayTable(buildSettingsName, "", "order by SETTING_NAME");
    // Display build settings object obtained from the model
    System.out.println("BuildSettings Details from the " 
      + buildSettingsName + " model build settings object:");    
    String objName = featureSettings.getName();
    if(objName != null)
      System.out.println("Name = " + objName);
    String objDescription = featureSettings.getDescription();
    if(objDescription != null)
      System.out.println("Description = " + objDescription);
    java.util.Date creationDate = featureSettings.getCreationDate();
    String creator = featureSettings.getCreatorInfo();     
    AlgorithmSettings algoSettings = featureSettings.getAlgorithmSettings();
    if(algoSettings == null)
      System.out.println("Failure: featureSettings.getAlgorithmSettings() returns null");
    MiningAlgorithm algo = algoSettings.getMiningAlgorithm();
    if(algo == null) System.out.println("Failure: algoSettings.getMiningAlgorithm() returns null");
    System.out.println("Algorithm Name: " + algo.name());
    MiningFunction function = featureSettings.getMiningFunction();
    if(function == null) System.out.println("Failure: featureSettings.getMiningFunction() returns null");
    System.out.println("Function Name: " + function.name()); 
    // List of NMF algorithm settings 
    long intValue = ((OraNMFAlgorithmSettings)algoSettings).getMaxNumberOfIterations();
    System.out.println("Max Number Of Iterations: " + intValue);
    double doubleValue = ((OraNMFAlgorithmSettings)algoSettings).getMinConvergenceTolerance();
    System.out.println("Min Convergence Tolerance: " + m_df.format(doubleValue));
    intValue = ((OraNMFAlgorithmSettings)algoSettings).getSeedValue();
    System.out.println("Seed Value: " + intValue);
    intValue = featureSettings.getNumberOfFeatures();
    System.out.println("Number Of Features: " + intValue);
  }

  /**
   * This method displayes the NMF model signature.
   * 
   *  @param model model object
   *  @exception JDMException
   */
  public static void displayModelSignature(Model model) throws JDMException
  {
    String modelName = model.getName();
    System.out.println("Model Name: " + modelName);
    // Display the first 10 rows only 
    ModelSignature modelSignature = model.getSignature();
    System.out.println("Model Signature: ( Attribute Name, Attribute Type )");
    MessageFormat mfSign = new MessageFormat("                        ( {0}, {1} )");
    String[] vals = new String[2]; 
    Collection sortedSet = modelSignature.getAttributes();
    Iterator attrIterator = sortedSet.iterator();

    /****************************************************************/
    // this part of code sorts the columns by name
    // to get the deterministic order of columns
    // this code can be omitted otherwise
    // sort the attributes by name
    TreeMap treeMap = new TreeMap();
    while(attrIterator.hasNext()) 
    {
      SignatureAttribute attr = (SignatureAttribute) attrIterator.next();
      String attrName = attr.getName();
      treeMap.put( attrName, attr );
    }
    Collection sortedSig = treeMap.values();
    attrIterator = sortedSig.iterator();
    /****************************************************************/

    while(attrIterator.hasNext()) 
    {
      SignatureAttribute attr = (SignatureAttribute)attrIterator.next();
      vals[0] = attr.getName();
      vals[1] = attr.getDataType().name();
      System.out.println( mfSign.format(vals) );
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

      /****************************************************************/
      // this part of code sorts the columns by name
      // to get the deterministic order of columns
      // this code can be omitted otherwise
      TreeMap treeMap = new TreeMap();
      for(int iCol=1; iCol<=colCount; iCol++) 
      {
        String colName = rsMeta.getColumnName(iCol);
        treeMap.put( colName, new Integer(iCol) );
      }
      Collection colColl = treeMap.values();
      int[] colIndex = new int[colCount];
      Iterator colIt = colColl.iterator();
      for( int i=0; colIt.hasNext(); i++ )
      {
        Integer index = (Integer) colIt.next();
        colIndex[i] = index.intValue();
      }
      /****************************************************************/

      // Build table header
      for(int iCol=1; iCol<=colCount; iCol++) 
      {
        String colName = rsMeta.getColumnName( colIndex[iCol-1] );
        // if no ordering is needed (with the above code block omitted),
        // this line can be used
        // String colName = rsMeta.getColumnName(iCol);
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
          int sqlType = rsMeta.getColumnType( colIndex[iCol-1] );          
          Object obj = rs.getObject( colIndex[iCol-1] );
          // if no ordering is needed (with the above code block omitted),
          // the following two lines can be used
          // int sqlType = rsMeta.getColumnType(iCol);          
          // Object obj = rs.getObject(iCol);
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
  
  /**
   * Each feature is a linear combination of the original attribute set; 
   * the coefficients of these linear combinations are non-negative.
   * The model details return for each feature the coefficients
   * associated with each one of the original attributes. Categorical 
   * attributes are described by (attribute_name, attribute_value) pairs.
   * That is, for a given feature, each distinct value of a categorical 
   * attribute has its own coefficient.
   * 
   * @param model model to be presented
   * @exception JDMException if failed to retrieve model details
   */
  public static void displayFEModelDetails(OraFeatureExtractionModel model) throws JDMException
  {
      System.out.println(UNDERLINE+"\n"+"NMF model details"+"\n"+UNDERLINE+"\n");
      System.out.println("\n"+TAB+"Listing all features:\n"+UNDERLINE);
      Collection features = model.getFeatures();
      if ( features == null ){
        System.out.println(TAB+TAB+"Error printing features.");
        return;
      }

      /****************************************************************/
      // this part of code sorts the features by id
      // to get the deterministic order of features
      // this code can be omitted otherwise
      // use tree map to sort the features/attributes for determinstic results
      OraFeature sampleFeature = null;
      Iterator it = features.iterator();

      TreeMap treeMap = new TreeMap();
      while( it.hasNext() )
      {
        OraFeature feature = (OraFeature)it.next();
        int featureId = feature.getFeatureIdentifier();
        treeMap.put( new Integer(featureId), feature );
      }
      Collection featuresOrdered = treeMap.values();
      it = featuresOrdered.iterator();
      /****************************************************************/

      while ( it.hasNext() ){
        OraFeature feature = (OraFeature)it.next();
        printFeature(feature);
        if ( sampleFeature == null )
          sampleFeature = feature;
      }
  
      System.out.println("\n"+TAB+"Listing Top 3 features:\n"+UNDERLINE);
      Collection topNfeatures = model.getFeatures(3);
      if ( topNfeatures == null ){
        System.out.println(TAB+TAB+"Error printing Top N features.");
        return;
      }

      Iterator itTopN = topNfeatures.iterator();
      while ( itTopN.hasNext() ){
        OraFeature feature = (OraFeature)itTopN.next();  
        printFeature(feature);
      }
      
      if ( sampleFeature != null ){
        System.out.println(TAB+"Listing all attribute names for the feature:" + sampleFeature.getFeatureIdentifier());
        System.out.println(TAB+"____________________________________________");
        String[] attrNames = sampleFeature.getAttributeNames();
        for ( int ni = 0 ; ni < attrNames.length ; ni++ )  {
          Map attrValCoefficientMap = sampleFeature.getAttributeCoefficients(attrNames[ni]);
          System.out.println("\n"+TAB+TAB+"Attribute Name:" + attrNames[ni] );
          printCoefficients(attrNames[ni], attrValCoefficientMap, false);
        }
      }
  }
  
  /**
   * Display a single feature
   * 
   * @param feature specific feature to be displayed
   * @throws JDMException if failed to retrieve the feature details
   */
  public static void printFeature(OraFeature feature) throws JDMException
  {
    if ( feature == null ){
      System.out.println("Error: feature is null");
      return;
    }
    
    System.out.println("\n" + TAB+TAB+"Feature : " + feature.getFeatureIdentifier() );
    String[] featureAttrNames = feature.getAttributeNames();
    //Print attributes coefficient details for each attribute
    if(featureAttrNames != null) {
      for(int iAttr=0; iAttr < featureAttrNames.length; iAttr++) 
      {
        Map attrValCoefficientMap = feature.getAttributeCoefficients(featureAttrNames[iAttr]);
        printCoefficients(featureAttrNames[iAttr], attrValCoefficientMap, true);
      }
    } 
  }
  
  
  
  private static void printCoefficients (String attrName, Map attrValCoefficientMap, boolean printAttrName) 
  throws JDMException
  {
    if ( attrValCoefficientMap == null ){
      System.out.println("Error printing coefficients for this feature.");
      return;
    }
    
    if ( printAttrName == true )
      print3Columns(new String[]{"Attribute","Value","Coefficient"} );
    else
      print3Columns(new String[]{"","Value","Coefficient"} );

    System.out.println(TAB+TAB+TAB +"____________________________________________________________________________" );
    Object[] attrVals = attrValCoefficientMap.keySet().toArray();
    if(attrVals != null) {
        for(int iAttr=0; iAttr < attrVals.length; iAttr++) 
        {                      
          print3Columns ( new String[]{
                         ( printAttrName == true ? attrName : "" ) , 
                         (( attrVals[iAttr] == null ? "" : attrVals[iAttr] )).toString(),
                         String.valueOf(m_df.format((Number)attrValCoefficientMap.get(attrVals[iAttr])))
                        }
                    );
        }
    }
    
  }
  
  private static void print3Columns(String[] names){
    if ( names == null || names.length != 3 )
      return;
      
    StringBuffer sbOut = new StringBuffer(TAB+TAB+TAB);
    sbOut.append(names[0]);
    
    int padLength = Math.max ( SECOND_COLUMN - sbOut.length(), 0 );
    for ( int ni = 0 ; ni < padLength; ni++)
      sbOut.append(SPACE);

    sbOut.append(names[1]);
    padLength = Math.max ( THIRD_COLUMN - sbOut.length(), 0 );
    for ( int ni = 0 ; ni < padLength; ni++)
      sbOut.append(SPACE);
    
    sbOut.append(names[2]);
    System.out.println(sbOut.toString());
  }
  
  private static void clean() 
  {
    java.sql.Connection dbConn = 
      ((OraConnection) m_dmeConn).getDatabaseConnection();
    Statement stmt = null;
    // Drop prepared tables
    try 
    {
      stmt = dbConn.createStatement();
      stmt.executeUpdate("DROP TABLE TXTNMF_BUILD_NESTED_TEXT");      
    } catch(SQLException anySqlExp) {}//Ignore
    finally{
      try {
        stmt.close();
      }
      catch(Exception anySqlExp){}
    }
    try 
    {
      stmt = dbConn.createStatement();
      stmt.executeUpdate("DROP TABLE TXTNMF_APPLY_NESTED_TEXT");      
    } catch(SQLException anySqlExp) {}//Ignore
    finally{
      try {
        stmt.close();
      }
      catch(Exception anySqlExp){}
    }
    // Drop prepared views
    try 
    {
      stmt = dbConn.createStatement();
      stmt.executeUpdate("DROP VIEW TXTNMF_NORM_DATA_BUILD_JDM");   
    } catch(SQLException anySqlExp) {}//Ignore
    finally{
      try {
        stmt.close();
      }
      catch(Exception anySqlExp){}
    }
    try 
    {
      stmt = dbConn.createStatement();
      stmt.executeUpdate("DROP VIEW TXTNMF_NORM_DATA_APPLY_JDM"); 
    } catch(SQLException anySqlExp) {}//Ignore
    finally{
      try {
        stmt.close();
      }
      catch(Exception anySqlExp){}
    }
    // Drop apply output table
    try 
    {
      stmt = dbConn.createStatement();
      stmt.executeUpdate("DROP TABLE TXTNMF_APPLY_OUTPUT_JDM");      
    } catch(SQLException anySqlExp) {}//Ignore
    finally{
      try {
        stmt.close();
      }
      catch(Exception anySqlExp){}
    }
    try {
      m_dmeConn.removeObject("txtnmfModel_jdm", NamedObject.model);
    } catch(JDMException jdmExp) {}
  }
}
