// Copyright (c) 2001, 2006, Oracle. All rights reserved.  
// File: dmocdemo.java

/**
* This demo program describes how to use the Oracle Data Mining (ODM) Java API 
* to cluster data records based on Orthogonal Clustering. 
* The o-Cluster (OC) clustering model provides the user with insight about the 
* discovered groups. 
* ------------------------------------------------------------------------------
*                             PROBLEM DEFINITION
* ------------------------------------------------------------------------------
*   Segment the demographic data into 10 clusters and study the individual
* clusters. Apply clustering model to new data and rank the clusters on probability.
* ------------------------------------------------------------------------------
*                             DATA DESCRIPTION
* ------------------------------------------------------------------------------
*   Data for this demo is composed from base tables in the Sales History (SH) 
* Schema. The SH schema is an Oracle Database Sample Schema that has the customer
* demographics, purchasing, and response details for the previous affinity card 
* programs. Data exploration and preparing the data is a common step before 
* doing data mining. Here in this demo, the following views are created 
* in the user schema using CUSTOMERS, COUNTRIES and SUPPLIMENTARY_DEMOGRAPHICS 
* tables.
* 
* MINING_DATA_BUILD_STR_V:
*   This view collects the previous customers' demographics, purchasing, and affinity 
*   card response details for building the model.
*   
* MINING_DATA_APPLY_STR_V:
*   This view collects the prospective customers' demographics and purchasing 
*   details for scoring against the clustering model.
*
* ------------------------------------------------------------------------------
*                             DATA MINING PROCESS
* ------------------------------------------------------------------------------
*   Prepare Data:
*     OC is not a distance based algorithm like K-Means, and instead works on 
*     histograms. Therefore binning is the preferred transformation for high 
*     cardinality data. OC uses a special binning procedure that automatically 
*     determines the number of bins based on data statistics. The 
*     prepareData() method illustrates the binning of the build and apply 
*     data.
*   Build Model:
*     Mining Model is the prime object in data mining. The buildModel() method
*     illustrates how to build a clustering model using the OC algorithm.
*   Apply Model:
*     The results of an APPLY operation will contain a list of the cluster 
*     identifiers for each case, and the associated probabilities. 
* ------------------------------------------------------------------------------
*                             EXECUTING DEMO PROGRAM
* ------------------------------------------------------------------------------
*   Refer to Oracle Data Mining Administrator's Guide 
*   for guidelines for executing this demo program.
*/
// Generic api imports
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Statement;
import java.util.Collection;
import java.util.Enumeration;
import java.util.Iterator;
import java.util.Set;
import java.util.TreeMap;
import java.util.Hashtable;
// Java Data Mining (JDM) standard imports
import javax.datamining.ExecutionHandle;
import javax.datamining.ExecutionState;
import javax.datamining.ExecutionStatus;
import javax.datamining.JDMException;
import javax.datamining.NamedObject;
import javax.datamining.base.Task;
import javax.datamining.clustering.Cluster;
import javax.datamining.clustering.ClusteringApplySettings;
import javax.datamining.clustering.ClusteringApplySettingsFactory;
import javax.datamining.clustering.ClusteringModel;
import javax.datamining.clustering.ClusteringSettings;
import javax.datamining.clustering.ClusteringSettingsFactory;
import javax.datamining.data.AttributeDataType;
import javax.datamining.data.AttributeType;
import javax.datamining.data.Interval;
import javax.datamining.data.IntervalClosure;
import javax.datamining.data.ModelSignature;
import javax.datamining.data.PhysicalAttribute;
import javax.datamining.data.PhysicalAttributeFactory;
import javax.datamining.data.PhysicalAttributeRole;
import javax.datamining.data.PhysicalDataSet;
import javax.datamining.data.PhysicalDataSetFactory;
import javax.datamining.data.SignatureAttribute;
import javax.datamining.resource.ConnectionSpec;
import javax.datamining.rule.CompoundPredicate;
import javax.datamining.rule.Predicate;
import javax.datamining.rule.Rule;
import javax.datamining.rule.SimplePredicate;
import javax.datamining.statistics.AttributeStatisticsSet;
import javax.datamining.statistics.UnivariateStatistics;
import javax.datamining.task.BuildTask;
import javax.datamining.task.BuildTaskFactory;
import javax.datamining.task.apply.DataSetApplyTask;
import javax.datamining.task.apply.DataSetApplyTaskFactory;
// Oracle Java Data Mining (JDM) implemented api imports
import oracle.dmt.jdm.OraPLSQLMappings;
import oracle.dmt.jdm.algorithm.ocluster.OraOClusterSettings;
import oracle.dmt.jdm.algorithm.ocluster.OraOClusterSettingsFactory;
import oracle.dmt.jdm.clustering.OraCluster;
import oracle.dmt.jdm.resource.OraConnection;
import oracle.dmt.jdm.resource.OraConnectionFactory;
import oracle.dmt.jdm.rule.OraSimplePredicate;
import oracle.dmt.jdm.task.OraTransformationTask;
import oracle.dmt.jdm.task.OraTransformationTaskFactory;
import oracle.dmt.jdm.transform.binning.OraBinningTransformFactory;
import oracle.dmt.jdm.transform.binning.OraBinningTransformImpl;
import oracle.dmt.jdm.transform.binning.OraCategoricalBinningType;
import oracle.dmt.jdm.transform.binning.OraNumericalBinningType;

public class dmocdemo extends Object{
  //Connection related data members
  private static javax.datamining.resource.Connection m_dmeConn;
  private static javax.datamining.resource.ConnectionFactory m_dmeConnFactory;
  //Object factories used in this demo program
  private static PhysicalDataSetFactory m_pdsFactory;
  private static PhysicalAttributeFactory m_paFactory;
  private static ClusteringSettingsFactory m_clusFactory;
  private static OraOClusterSettingsFactory m_oclusterFactory;
  private static BuildTaskFactory m_buildFactory;
  private static DataSetApplyTaskFactory m_dsApplyFactory;
  private static ClusteringApplySettingsFactory m_applySettingsFactory;
  private static OraBinningTransformFactory m_binningXformFactory;
  private static OraTransformationTaskFactory m_xformTaskFactory;
  // Global constants used for formatting output
  private static String TAB = "    ";
  private static String CR = "\n";
  private static String CR_TAB = "\n    ";
  private static String HORZ_LINE = "----";
  private static String UNDERLINE                       = "*************************************";
  private static String LEAF_CLUSTERS_HEADER            = "*             Leaf clusters         *";
  private static String RULES_CLUSTERS_HEADER           = "*             Rules                 *";
  private static String RULES_CLUSTERS_HIERARCHY_HEADER = "*     Printing clusters hierarchy   *";
  
  public static void main( String args[] ) { 
    try {      
        if (( args.length != 0 ) & ( args.length != 3 )) {
          System.out.println("Usage: java dmocdemo ");
          System.out.println("   or: java dmocdemo <Host name>:<Port>:<SID> <User Name> <Password>");
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
    m_clusFactory = (ClusteringSettingsFactory)m_dmeConn.getFactory(
      "javax.datamining.clustering.ClusteringSettings");
    m_oclusterFactory = (OraOClusterSettingsFactory)m_dmeConn.getFactory(
      "oracle.dmt.jdm.algorithm.ocluster.OraOClusterSettings");
    m_buildFactory = (BuildTaskFactory)m_dmeConn.getFactory(
      "javax.datamining.task.BuildTask");
    m_dsApplyFactory = (DataSetApplyTaskFactory)m_dmeConn.getFactory(
      "javax.datamining.task.apply.DataSetApplyTask");
    m_applySettingsFactory = (ClusteringApplySettingsFactory)m_dmeConn.getFactory(
      "javax.datamining.clustering.ClusteringApplySettings");
    m_binningXformFactory = (OraBinningTransformFactory)m_dmeConn.getFactory(
      "oracle.dmt.jdm.transform.binning.OraBinningTransform");
    m_xformTaskFactory = (OraTransformationTaskFactory)m_dmeConn.getFactory(
      "oracle.dmt.jdm.task.OraTransformationTask");
  }

  /**
   *   This method illustrates preparation of the data for build and apply 
   * operations by using binning transformation.  
   * 
   *   Here all numerical attributes except CUST_ID are prepared using 
   * auto equal width binning. Categorical attributes are not binned.
   * For more details about the binning please refer to 
   * Oracle Data Mining Concepts Guide.
   * 
   *   The case id and any attribute with <= 2 distinct values should be excluded 
   * from the binning process.   
   * 
   *   The following table illustrates customer attributes available for building
   * the mining model and data characteristics and type of binning used
   * for each attribute and number of bins used.
   * 
   *   COLUMN_NAME              DATA_TYPE  DISTINCT EXCLUDED  BIN TYPE     BIN #
   *   ------------------       ---------  -------- --------  -----------  -----
   *   CUST_ID                  NUMBER     1500     YES
   *   AGE                      NUMBER     66       NO        AUTOEQWIDTH   10
   *   YRS_RESIDENCE            NUMBER     15       NO        AUTOEQWIDTH   10
   *   
   *   Build data binnning transformation produces numerical and categorical 
   * bin boundary tables. These tables must be specified for apply data
   * to bin the data consistent with the build data.
   *   Following binned tables are created after the execution of this method. 
   * 
   *   Unprepared Data        --->  Prepared(Binned) Data
   *   ---------------------        ---------------------------
   *   MINING_DATA_BUILD_STR_V      OC_BINNED_DATA_BUILD_JDM
   *   MINING_DATA_APPLY_STR_V      OC_BINNED_DATA_APPLY_JDM
   */
  public static void prepareData() throws JDMException 
  {
      boolean isOutputAsView = false;
      String inputDataURI = null;
      String outputDataURI = null;
      String categoricalBinTable = null;
      String numericalBinTable = null;
      OraTransformationTask xformTask = null;
      
      // Prepare build data
      isOutputAsView = true;
      inputDataURI = "MINING_DATA_BUILD_STR_V";
      outputDataURI = "OC_BINNED_DATA_BUILD_JDM";
      // Create boundaries for all numeric attributes
      OraBinningTransformImpl buildDataXform = 
          (OraBinningTransformImpl)m_binningXformFactory.create(
                              inputDataURI, outputDataURI, isOutputAsView );
      buildDataXform.setLiteralFlag(true);
      String[] excludeColumnList = {"CUST_ID"};  
      buildDataXform.setExcludeColumnList(excludeColumnList);
      buildDataXform.setNumberOfBinsForNumerical(10);
      buildDataXform.setNumericalBinningType(OraNumericalBinningType.auto_equi_width);
      buildDataXform.setCategoricalBinningType(OraCategoricalBinningType.systemDefault);
      xformTask = m_xformTaskFactory.create(buildDataXform);
      executeTask(xformTask, "ocPrepareBuildTask_jdm");    
       
      // Prepare apply data
      isOutputAsView = true;
      inputDataURI = "MINING_DATA_APPLY_STR_V";
      outputDataURI = "OC_BINNED_DATA_APPLY_JDM";
      categoricalBinTable = buildDataXform.getCategoricalBinTable();
      numericalBinTable = buildDataXform.getNumericalBinTable();
      OraBinningTransformImpl applyDataXform = 
          (OraBinningTransformImpl)m_binningXformFactory.create(
                              inputDataURI, outputDataURI, isOutputAsView, 
                              categoricalBinTable, numericalBinTable);
      applyDataXform.setLiteralFlag(true);
      xformTask = m_xformTaskFactory.create(applyDataXform);
      executeTask(xformTask, "ocPrepareApplyTask_jdm");  
  }
  
  /**
   *   This method illustrates how to build a mining model using the
   * "OC_BINNED_DATA_BUILD_JDM" dataset with the OC algorithm.
   * 
   * After completing the build task, the model named "ocModel_jdm" will 
   * be created.
   * 
   * @exception JDMException if model build failed
   */
  public static void buildModel() throws JDMException 
  {
    //1. Create & save PhysicalDataSpecification      
      PhysicalDataSet buildData = m_pdsFactory.create("OC_BINNED_DATA_BUILD_JDM", false);
      PhysicalAttribute pa = m_paFactory.create("CUST_ID", 
        AttributeDataType.integerType, PhysicalAttributeRole.caseId );
      buildData.addAttribute(pa);
      m_dmeConn.saveObject("ocBuildData_jdm", buildData, true);
    //2. Create & save Mining Function Settings
      //Create oClusetr algorithm settings
      OraOClusterSettings ocAlgo = (OraOClusterSettings)m_oclusterFactory.create();
      ocAlgo.setBufferSize(30000);
      //ocAlgo.setSensitivity(0.5);
      
      //Create ClusteringSettings
      ClusteringSettings buildSettings = m_clusFactory.create();
      buildSettings.setAlgorithmSettings(ocAlgo);
      buildSettings.setMaxNumberOfClusters(10);
      m_dmeConn.saveObject("ocBuildSettings_jdm", buildSettings, true);
    //3. Create, save & execute Build Task      
      BuildTask buildTask = m_buildFactory.create(
                     "ocBuildData_jdm", //Build data specification
                     "ocBuildSettings_jdm", //Mining function settings name
                     "ocModel_jdm" //Mining model name
                     );                          
      buildTask.setDescription("ocBuildTask_jdm");
      executeTask(buildTask, "ocBuildTask_jdm"); 
    //4. Restore the model from the DME and explore the details of the model
    ClusteringModel model = (ClusteringModel)
      m_dmeConn.retrieveObject("ocModel_jdm", NamedObject.model);
    displayOCModelDetails(model);
  }        
  
  /**
   *  For a descriptive mining function like Clustering, "Scoring" involves
   * assigning the probability with which a given case belongs to a given
   * cluster.
   * 
   * After completing the apply task, an apply output table 
   * "oc_apply_output_jdm" will be created at the user specfied location.
   * 
   * @exception JDMException if model apply failed
   */
  public static void applyModel() throws JDMException
  {    
      //1. Create & save PhysicalDataSpecification      
      PhysicalDataSet applyData = m_pdsFactory.create(
        "OC_BINNED_DATA_APPLY_JDM", false );
      PhysicalAttribute pa = m_paFactory.create("CUST_ID", 
        AttributeDataType.integerType, PhysicalAttributeRole.caseId );
      applyData.addAttribute( pa );
      m_dmeConn.saveObject( "ocApplyData_jdm", applyData, true );
      //2. Create & save ClassificationApplySettings
      ClusteringApplySettings clusAS = m_applySettingsFactory.create();
      m_dmeConn.saveObject( "ocApplySettings_jdm", clusAS, true);
            
      //3. Create, store & execute apply Task
      DataSetApplyTask applyTask = m_dsApplyFactory.create(
          "ocApplyData_jdm", 
          "ocModel_jdm", 
          "ocApplySettings_jdm", "oc_apply_output_jdm");
      executeTask(applyTask, "ocApplyTask_jdm");
      //4. Display results
      displayScoringResults();
  } 
  
  /**
   * Shows scoring results. Lists the clusters into which the customers in this 
   * dataset have been grouped. 
   */
  public static void displayScoringResults()
  {
      String sqlResults = 
        "SELECT clus, COUNT(*) AS CNT FROM " + 
        " (SELECT cluster_id CLUS, " +
        "   ROW_NUMBER() OVER " +
        "          (PARTITION BY CUST_ID ORDER BY PROBABILITY DESC) CLUS_RNK " +
        " FROM oc_apply_output_jdm) " +
        "WHERE CLUS_RNK = 1 " +
        "GROUP BY CLUS ORDER BY CNT DESC";


      Statement stmt = null;
      ResultSet rs = null;
      java.sql.Connection dbConn = 
        ((OraConnection)m_dmeConn).getDatabaseConnection();
      try {
        stmt = dbConn.createStatement();
        rs = stmt.executeQuery(sqlResults); 
        System.out.println("Cluster ID Count");
        System.out.println("----------------------------");
        while ( rs.next() ) {
          int clus = rs.getInt(1);
          int cnt  = rs.getInt(2);
          System.out.println(TAB + clus + TAB + TAB + cnt);
        }
      } catch(SQLException anySqlExp) {
        System.out.println(anySqlExp);
      } 
      finally{
        try {
          stmt.close();
        }
        catch(Exception anySqlExp){}
      }
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

  private static void clean() 
  {
     //Drop prepared views
    java.sql.Connection dbConn = 
      ((OraConnection)m_dmeConn).getDatabaseConnection();
    Statement stmt = null;
    //Drop the model
    try {
      m_dmeConn.removeObject("ocModel_jdm", NamedObject.model);
    } catch(JDMException jdmExp) {}
    finally{
      try {
        stmt.close();
      }
      catch(Exception anySqlExp){}
    }
    try 
    {
      stmt = dbConn.createStatement();
      stmt.executeUpdate("DROP VIEW OC_BINNED_DATA_BUILD_JDM");   
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
      stmt.executeUpdate("DROP VIEW OC_BINNED_DATA_APPLY_JDM"); 
    } catch(SQLException anySqlExp) {}//Ignore
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
      stmt.executeUpdate("DROP TABLE OC_APPLY_OUTPUT");      
    } catch(SQLException anySqlExp) {}//Ignore
    finally{
      try {
        stmt.close();
      }
      catch(Exception anySqlExp){}
    }
  }
  
  /**
   * This method displayes OC model details.
   * 
   * @param model to be presented
   * 
   * @exception JDMException if failed to retrieve model details
   */  
  public static void displayOCModelDetails(ClusteringModel model) throws JDMException{
    String modelName = model.getName();
    System.out.println("Model Name: " + modelName);
    System.out.println("Clustering model details:");
    System.out.println(TAB+"Number of clusters: "+ model.getNumberOfClusters());
    System.out.println(TAB+"Number of tree levels: "+ model.getNumberOfLevels());
    Collection clusters = model.getClusters();
    if ( clusters == null || clusters.isEmpty() == true ){
      System.out.println("Unable to retrieve clusters.");
      return;
    }
    
    // Display root cluster(s)
    Collection vRootClusters = model.getRootClusters(); 
    Cluster[] rootCluster = null;
    if ( vRootClusters != null && vRootClusters.isEmpty() == false ){
      rootCluster =(Cluster[])vRootClusters.toArray(new Cluster[vRootClusters.size()]);
      if ( rootCluster != null ){
        System.out.println(TAB+"Root Cluster Id: " + rootCluster[0].getClusterId());
      }
    }
    
    // Display leaf clusters
    Collection vLeafClusters = model.getLeafClusters();
    if ( vLeafClusters != null && vLeafClusters.isEmpty() == false ){
      Cluster[] leafClusters =(Cluster[])vLeafClusters.toArray(new Cluster[vLeafClusters.size()]);
      System.out.println(UNDERLINE);
      System.out.println(LEAF_CLUSTERS_HEADER);
      System.out.println(UNDERLINE);
      for ( int ni = 0; ni < leafClusters.length ; ni++ )  {
        printSingleClusterDetails(leafClusters[ni]);
        if ( ni == 0 ){
          // display first leaf cluster statistics
          printClusterStatistics(model, leafClusters[ni], 1);          
        }
      }
    }
    
    // Display all model rules
    Collection vRules = model.getRules();
    if ( vRules != null && vRules.isEmpty() == false ){
      Rule[] rules =(Rule[])vRules.toArray(new Rule[vRules.size()]);
      System.out.println(CR+CR+UNDERLINE);
      System.out.println(RULES_CLUSTERS_HEADER);
      System.out.println(UNDERLINE);
      for ( int rl = 0; rl < rules.length ; rl++ )  {
        printRuleNoDetails(rules[rl], 0);
      }
    }
    
    if ( rootCluster != null ){
      // print hierarchy
      System.out.println(CR+CR+UNDERLINE);
      System.out.println(RULES_CLUSTERS_HIERARCHY_HEADER);
      System.out.println(UNDERLINE);
      // 1.print root
      printRootClusterDetails(rootCluster[0]);
      // 1.print children
      Cluster[] children = rootCluster[0].getChildren();
      int indent = 0;
      if ( children != null ){
        for (int k=0 ; k<children.length ; k++ ){
          printRecursiveClusterDetails(model, children[k], indent);
        }
      }
      return;
    }
      
    // print flat structure
    Cluster[] arrayClusters = (Cluster[])clusters.toArray(new Cluster[clusters.size()]);
    for ( int ni = 0; ni < arrayClusters.length ; ni++ )  {
      printSingleClusterDetails(arrayClusters[ni]);
    }
    System.out.println(CR+UNDERLINE+CR);
  }
  

  private static String getIndentation(int indent, String sText){
    StringBuffer sbIndent = new StringBuffer(TAB);
    for ( int in = 0; in < indent; in++ )
      sbIndent.append(TAB);
    
    StringBuffer outPut = new StringBuffer (sbIndent.toString());
    outPut.append(sText);
    return outPut.toString();
  }

  /**
   * This method recursively displayes Cluster details.
   * 
   * @param model model to be presented
   * @param cluster current cluster which details are being presented
   * @param indent indentation level to illustrate cluster parent-child relation
   * 
   * @exception JDMException if failed to retrieve cluster details
   */  
  public static void printRecursiveClusterDetails(ClusteringModel model, Cluster cluster, int indent) throws JDMException{
    indent++;
    System.out.println(getIndentation (indent, "Cluster Id: " + cluster.getClusterId()) + 
        CR+ getIndentation (indent,"Case Count: " + cluster.getCaseCount()) +
        CR+ getIndentation (indent, "Tree Level: " + cluster.getLevel()) + 
        CR+ getIndentation (indent, "Dispersion: " + ((OraCluster)cluster).getDispersion()) + 
        CR+ getIndentation (indent, "Parent's id: " + ( cluster.getParent() != null ? 
          String.valueOf(cluster.getParent().getClusterId()) : "" ) )
      );
    
    Cluster[] ancestors = cluster.getAncestors();
    if ( ancestors != null ){
      StringBuffer sbTab = new StringBuffer("Anchestors");
      if ( ancestors.length == 0 )
        sbTab.append(" : None");
      for (int j=0 ; j<ancestors.length ; j++ ){
        for ( int sp = 0; sp < j; sp++ )
          sbTab.append(":" + ancestors[j].getClusterId() + " ");
      }
      System.out.print(getIndentation (indent, sbTab.toString() ) ); 
    }
    
    Cluster[] children = cluster.getChildren();
    if ( children != null ){
      System.out.println(CR+ getIndentation (indent,"Children:") );
      for (int k=0 ; k<children.length ; k++ ){
        Cluster childCluster = children[k];
        printRecursiveClusterDetails(model, childCluster, indent);
      }
    }
    else{
      System.out.print(CR+ getIndentation (indent,"No child clusters"+CR) );
      Rule rule = cluster.getRule();
      //Print Rule Details for leaf clusters only
      printRuleDetails(rule, indent);
    }
    indent--;
  }
  
  /**
   * This method prints Rules details with antecedent and consequent information.
   * 
   * @param rule rule which details are being presented
   * @param indent indentation level to illustrate cluster parent-child relation
   * 
   * @exception JDMException if failed to retrieve cluster details
   */  
  public static void printRuleDetails(Rule rule, int indent){
    System.out.println (  
        CR_TAB+ getIndentation ( indent,"Rule number:" + rule.getRuleIdentifier() ) + 
        CR_TAB + TAB + getIndentation ( indent, "Support: "    + rule.getSupport() )+ 
        CR_TAB + TAB + getIndentation ( indent, "Confidence: " + rule.getConfidence() )
        );
        
     CompoundPredicate antecedent = (CompoundPredicate)rule.getAntecedent();
     if ( antecedent != null ){
       System.out.println( CR_TAB + TAB + getIndentation ( indent,("Antecedent: ")));
       printAntecedent(antecedent, indent);
      }
     CompoundPredicate consequent = (CompoundPredicate)rule.getConsequent();
     if ( consequent != null  ){
       System.out.println( CR_TAB + TAB + getIndentation ( indent,("Consequent: ")));
       printConsequent(consequent, indent);
     }
  }

  /**
   * This method prints Rules details without antecedent and consequent information.
   * 
   * @param rule rule which details are being presented
   * @param indent indentation level to illustrate cluster parent-child relation
   */  
  public static void printRuleNoDetails(Rule rule, int indent){
    System.out.println (  
        CR_TAB+ getIndentation ( indent,"Rule number:" + rule.getRuleIdentifier() ) + 
        CR_TAB + TAB + getIndentation ( indent, "Support: "    + rule.getSupport() )+ 
        CR_TAB + TAB + getIndentation ( indent, "Confidence: " + rule.getConfidence() )
        );
  }

  /**
   * This method prints antecedent information.
   * 
   * @param predicate antecedent being presented
   * @param indent indentation level to illustrate cluster parent-child relation
   * 
   * @exception JDMException if failed to retrieve antecedent details
   */  
  public static void printAntecedent(CompoundPredicate predicate, int indent) {
    try{
      SimplePredicate[] sps = (SimplePredicate[])predicate.getPredicates();
      if ( sps == null )
        return;
      
      // combine predicates by attribute name
      Hashtable htNamePredicateMap = new Hashtable();
      for ( int i = 0 ; i < sps.length; i++ ){
        StringBuffer simplePredicate = printSimplePredicate(sps[i]);
        String attrName = sps[i].getAttributeName();
        StringBuffer attrTotalPredicate = (StringBuffer)htNamePredicateMap.get(attrName);
        if ( attrTotalPredicate == null )
          htNamePredicateMap.put(attrName, simplePredicate);
        else{
          attrTotalPredicate.append(" AND " + simplePredicate);
        }
      }
      Enumeration en = htNamePredicateMap.keys();
      while ( en.hasMoreElements() ){
        String name = (String)en.nextElement();
        StringBuffer sb = (StringBuffer)htNamePredicateMap.get(name);
        System.out.println( getIndentation ( indent,(TAB+TAB+TAB+sb.toString())));
      }
    }
    catch(Exception e){
      System.out.println("Error printing Antecedant");
    }
  }

  /**
   * This method prints consequent information.
   * 
   * @param predicate consequent being presented
   * @param indent indentation level to illustrate cluster parent-child relation
   * 
   * @exception JDMException if failed to retrieve consequent details
   */  
  public static void printConsequent(CompoundPredicate predicate, int indent) {
    try{
      SimplePredicate[] sps = (SimplePredicate[])predicate.getPredicates();
      if ( sps == null )
        return;
      
      for ( int i = 0 ; i < sps.length; i++ ){
        StringBuffer simplePredicate = printSimplePredicate(sps[i]);
        if ( i < sps.length - 1)
          simplePredicate.append( " AND ");
        System.out.println( getIndentation ( indent,TAB+TAB+TAB+simplePredicate.toString()));
      }
    }
    catch(Exception e){
      System.out.println("Error printing Consequent");
    }
  }
  
  /**
   * This method prints <code>SimplePredicate</code> information.
   * 
   * @param predicate <code>SimplePredicate</code> being presented
   * 
   * @exception JDMException if failed to retrieve <code>SimplePredicate</code> details
   */  
  public static StringBuffer printSimplePredicate(SimplePredicate predicate) throws JDMException{
    StringBuffer sb = new StringBuffer();
    if(predicate == null){
      sb.append(" ");
      return sb;
    }
    if ( predicate.isNumericalValue() ){
      sb.append( 
        predicate.getAttributeName() + " " + 
        //((OraSimplePredicate)predicate).getComparisonOperatorValue() + " " + 
        OraPLSQLMappings.getComparisonOperatorValue_tree(((OraSimplePredicate)predicate).getComparisonOperator()) + " " + 
        predicate.getNumericalValue()
      );
    }
    else{
      sb = new StringBuffer( 
        predicate.getAttributeName() + " " + 
        //((OraSimplePredicate)predicate).getComparisonOperatorValue() + " "
        OraPLSQLMappings.getComparisonOperatorValue_tree(((OraSimplePredicate)predicate).getComparisonOperator()) + " " 
      );
      Object[] inValues = predicate.getCategoryValues();
      if ( inValues != null ){
        for (int i=0 ; i < inValues.length; i++ )  {
          sb.append(inValues[i] );
          if ( i != inValues.length - 1)
            sb.append(",");
        }
      }
    }
    return sb;
  }
  
  /**
   * This method shows details of the root cluster.
   * 
   * @param cluster root cluster
   * 
   * @exception JDMException if failed to retrieve root cluster details
   */  
  public static void printRootClusterDetails(Cluster cluster) throws JDMException{
    CompoundPredicate cp = (CompoundPredicate)cluster.getSplitPredicate();
    Predicate[] predicates = cp.getPredicates();
    for ( int ni = 0;  ni < predicates.length; ni++) {
      StringBuffer sb = printSimplePredicate((SimplePredicate)predicates[ni]);
      System.out.println(CR+"Root Cluster Id: " + cluster.getClusterId() + 
          CR_TAB + "Case Count: " + cluster.getCaseCount() +
          CR_TAB + "Tree Level: " + cluster.getLevel() + 
          CR_TAB + "Dispersion: " + ((OraCluster)cluster).getDispersion() +
          CR_TAB + "Split predicate: " + sb.toString() +
          CR_TAB + "Children:"
        );
    }
  }
  
  /**
   * This method shows details of any cluster.
   * 
   * @param cluster cluster being presented
   * 
   * @exception JDMException if failed to retrieve this cluster details
   */  
  public static void printSingleClusterDetails(Cluster cluster) throws JDMException{
    System.out.println(CR+"Cluster Id: " + cluster.getClusterId() + 
        CR_TAB+"Case Count: " + cluster.getCaseCount() +
        CR_TAB+"Tree Level: " + cluster.getLevel() + 
        CR_TAB+"Dispersion: " + ((OraCluster)cluster).getDispersion() + 
        CR_TAB+"Parent's id: " + ( cluster.getParent() != null ? 
          String.valueOf(cluster.getParent().getClusterId()) : "") +
        CR_TAB + "Is root Cluster: " + cluster.isRoot() +
        CR_TAB + "Is leaf Cluster: " + cluster.isLeaf()
      );
    
    Cluster[] ancestors = cluster.getAncestors();
    StringBuffer sbTab = new StringBuffer(TAB+"Anchestors ");
    if ( ancestors != null ){
      for (int j=0 ; j<ancestors.length ; j++ ){
        for ( int sp = 0; sp < j; sp++ )
          sbTab.append(":" + ancestors[j].getClusterId() + " ");
      }
    }
    else{
      sbTab.append("None");
    }
    System.out.print(sbTab.toString() ); 
    
    Cluster[] children = cluster.getChildren();
    System.out.println(CR_TAB+"Children:");
    if ( children != null ){
      for (int k=0 ; k<children.length ; k++ ){
        Cluster childCluster = children[k];
        System.out.print(TAB+TAB+"Child: " + childCluster.getClusterId() +CR ); 
      }
      System.out.println(CR);
    }
    else{
        System.out.print(TAB+TAB+"None" ); 
    }
  }
  
  /**
   * This method shows cluster statistics.
   * 
   * @param model model being presented
   * @param cluster cluster being presented
   * @param indent indentation level to illustrate cluster parent-child relation
   * 
   * @exception JDMException if failed to retrieve this cluster statistics
   */  
  public static void printClusterStatistics(ClusteringModel model, Cluster cluster, int indent){
    try{
      ModelSignature modelSignature = model.getSignature();
    
      AttributeStatisticsSet attrStat = cluster.getStatistics();
      if ( attrStat == null )
        return;
      TreeMap centroids = ((OraCluster)cluster).getCentroids();
      if ( centroids == null || centroids.isEmpty() ){
        System.out.println("Error: cluster " + cluster.getClusterId() + 
          " does not contain statsitics");
        return;
      }
      Set centroidKeys = centroids.keySet();
      Iterator attributesNames = centroidKeys.iterator();
       while ( attributesNames.hasNext() )  {
        String attributeName = (String)attributesNames.next();
        SignatureAttribute sa = modelSignature.getAttribute(attributeName);
        AttributeType aType = sa.getAttributeType();
        UnivariateStatistics stats =  attrStat.getStatistics(attributeName);  
        double[] frequencies = stats.getProbabilities();
        System.out.println(CR_TAB+getIndentation (indent,"Statistics for attribute: " + attributeName) ); 
        System.out.println(TAB+getIndentation (indent,UNDERLINE));
        if ( aType.equals(AttributeType.numerical) ){
          Interval[] ranges = (Interval[])stats.getValues();
          if ( ranges != null ) {
            System.out.println(TAB+TAB+getIndentation (indent,"Bin Id" + TAB+TAB+TAB+ "Range" + TAB+TAB+TAB+TAB+ "Frequency") ); 
            for ( int in = 0 ; in < ranges.length ; in++ )  {
              Interval bin = ranges[in];
              IntervalClosure closure = bin.getIntervalClosure();
              String openParenthesis = ( ( closure.equals(IntervalClosure.openClosed) || 
                                         closure.equals(IntervalClosure.openOpen ) ) ? "( " : "[ " );
              String closeParenthesis = ( ( closure.equals(IntervalClosure.openOpen) || 
                                         closure.equals(IntervalClosure.closedOpen ) ) ? " )" : " ]" );
              System.out.println( 
                TAB+TAB + getIndentation (indent, String.valueOf(in+1) ) +  // bin Id
                TAB+TAB + getIndentation (indent, openParenthesis + bin.getStartPoint() + " - " + bin.getEndPoint() + closeParenthesis) + //range
                TAB+TAB + getIndentation (indent, String.valueOf((int)frequencies[in] ) ) //frequency
                );
            }
          }
        }
        else if ( aType.equals(AttributeType.categorical) ){
          Object[] values = stats.getValues();
          if ( values != null ) {
            System.out.println(TAB+TAB+getIndentation (indent,"Bin Id" + TAB+"Category" + TAB+"Frequency") ); 
            for ( int in = 0 ; in < values.length ; in++ )  {
              System.out.println( 
                TAB+TAB + getIndentation (indent, String.valueOf(in+1) ) +  // bin Id
                TAB+TAB + values[in] + //value
                TAB+TAB + getIndentation (indent, String.valueOf((int)frequencies[in] ) ) //frequency
                );
            }
          }
        }
      }
    }
    catch(Exception e){
      System.out.println("Error printing statsitics for cluster: " + cluster.getClusterId());
    }
  }
}
