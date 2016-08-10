// Copyright (c) 2001, 2006, Oracle. All rights reserved.
// File: dmkmdemo.java
// Generic api imports
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Statement;

import java.util.Collection;
import java.util.Enumeration;
import java.util.Hashtable;
import java.util.Iterator;
import java.util.Set;
import java.util.TreeMap;

import javax.datamining.ExecutionHandle;
import javax.datamining.ExecutionState;
import javax.datamining.ExecutionStatus;
import javax.datamining.JDMException;
import javax.datamining.NamedObject;
import javax.datamining.algorithm.kmeans.ClusteringDistanceFunction;
import javax.datamining.algorithm.kmeans.KMeansSettingsFactory;
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
import javax.datamining.rule.Rule;
import javax.datamining.rule.SimplePredicate;
import javax.datamining.statistics.AttributeStatisticsSet;
import javax.datamining.statistics.UnivariateStatistics;
import javax.datamining.task.BuildTask;
import javax.datamining.task.BuildTaskFactory;
import javax.datamining.task.apply.DataSetApplyTask;
import javax.datamining.task.apply.DataSetApplyTaskFactory;

import oracle.dmt.jdm.OraPLSQLMappings;
import oracle.dmt.jdm.algorithm.kmeans.OraKMeansSettings;
import oracle.dmt.jdm.algorithm.kmeans.OraSplitCriterion;
import oracle.dmt.jdm.base.OraBuildSettings;
import oracle.dmt.jdm.base.OraTask;
import oracle.dmt.jdm.clustering.OraCluster;
import oracle.dmt.jdm.resource.OraConnection;
import oracle.dmt.jdm.resource.OraConnectionFactory;
import oracle.dmt.jdm.rule.OraSimplePredicate;
import oracle.dmt.jdm.task.OraTransformationTask;
import oracle.dmt.jdm.task.OraTransformationTaskFactory;
import oracle.dmt.jdm.transform.normalize.OraNormalizeTransformFactory;
import oracle.dmt.jdm.transform.normalize.OraNormalizeTransformImpl;
import oracle.dmt.jdm.transform.normalize.OraNormalizeType;


// Java Data Mining (JDM) standard imports
// Oracle Java Data Mining (JDM) implemented api imports

/**
* This demo program describes how to use the Oracle Data Mining (ODM) Java API
* to cluster data records based on their Eucledian distance similarity.
* The k-Means (KM) clustering model provides the user with insight about the
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
* MINING_DATA_BUILD_V:
*   This view collects the previous customers' demographics, purchasing, and affinity
*   card response details for building the model.
*
* MINING_DATA_APPLY_V:
*   This view collects the prospective customers' demographics and purchasing
*   details for scoring against the clustering model.
*
* ------------------------------------------------------------------------------
*                             DATA MINING PROCESS
* ------------------------------------------------------------------------------
*   Prepare Data:
*     In general, attributes need to be similar scale in order to be treated
*     equally during the model build. Therefore normalization is required.
*     The prepareData() method illustrates the normalization of the build and apply
*     data.
*   Build Model:
*     Mining Model is the prime object in data mining. The buildModel() method
*     illustrates how to build a clustering model using KM algorithm.
*   Apply Model:
*     The results of an APPLY operation will contain a list of the cluster
*     identifiers for each case, and the associated probabilities.
* ------------------------------------------------------------------------------
*                             EXECUTING DEMO PROGRAM
* ------------------------------------------------------------------------------
*   Refer to Oracle Data Mining Administrator's Guide
*   for guidelines for executing this demo program.
*/
public class

dmkmdemo
  extends Object
{
  //Connection related data members
  private static javax.datamining.resource.Connection m_dmeConn;
  private static javax.datamining.resource.ConnectionFactory m_dmeConnFactory;
  //Object factories used in this demo program
  private static PhysicalDataSetFactory m_pdsFactory;
  private static PhysicalAttributeFactory m_paFactory;
  private static ClusteringSettingsFactory m_clusFactory;
  private static KMeansSettingsFactory m_kmeansFactory;
  private static BuildTaskFactory m_buildFactory;
  private static DataSetApplyTaskFactory m_dsApplyFactory;
  private static ClusteringApplySettingsFactory m_applySettingsFactory;
  private static OraNormalizeTransformFactory m_normalizeXformFactory;
  private static OraTransformationTaskFactory m_xformTaskFactory;
  // Global constants used for formatting output
  private static String TAB = "    ";
  private static String CR = "\n";
  private static String CR_TAB = "\n    ";
  private static String HORZ_LINE = "----";
  private static String UNDERLINE =
    "*************************************";
  private static String LEAF_CLUSTERS_HEADER =
    "*             Leaf clusters         *";
  private static String RULES_CLUSTERS_HEADER =
    "*             Rules                 *";
  private static String RULES_CLUSTERS_HIERARCHY_HEADER =
    "*     Printing clusters hierarchy   *";

  public static void main(String[] args)
  {
    try
    {
      if ((args.length != 0) & (args.length != 3))
      {
        System.out.println("Usage: java dmkmdemo ");
        System.out.println("   or: java dmkmdemo <Host name>:<Port>:<SID> <User Name> <Password>");
        return;
      }
      String uri = args[0];
      String name = args[1];
      String password = args[2];
      //1. Login to the Data Mining Engine
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
      // 4. Save task to build a model
      buildModel();
      // 5. Save task to apply a model
      applyModel();
      // 6. Execute build task and apply task follows
      m_dmeConn.execute("kmBuildTask_jdm");
      //7. Monitor execution of the tasks
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
        // 7. Logout from the Data Mining Engine
        m_dmeConn.close();
      }
      catch (Exception anyExp1)
      {
      } //Ignore
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
    m_clusFactory =
        (ClusteringSettingsFactory) m_dmeConn.getFactory("javax.datamining.clustering.ClusteringSettings");
    m_kmeansFactory =
        (KMeansSettingsFactory) m_dmeConn.getFactory("javax.datamining.algorithm.kmeans.KMeansSettings");
    m_buildFactory =
        (BuildTaskFactory) m_dmeConn.getFactory("javax.datamining.task.BuildTask");
    m_dsApplyFactory =
        (DataSetApplyTaskFactory) m_dmeConn.getFactory("javax.datamining.task.apply.DataSetApplyTask");
    m_applySettingsFactory =
        (ClusteringApplySettingsFactory) m_dmeConn.getFactory("javax.datamining.clustering.ClusteringApplySettings");
    m_normalizeXformFactory =
        (OraNormalizeTransformFactory) m_dmeConn.getFactory("oracle.dmt.jdm.transform.normalize.OraNormalizeTransform");
    m_xformTaskFactory =
        (OraTransformationTaskFactory) m_dmeConn.getFactory("oracle.dmt.jdm.task.OraTransformationTask");
  }

  /**
   *   This method illustrates how to build a clustering mining model using
   * the kMeans algorithm.
   *
   * After completing the build task, the model named "kmModel_jdm" will
   * be created.
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
      m_paFactory.create("cust_id", AttributeDataType.integerType,
                         PhysicalAttributeRole.caseId);
    buildData.addAttribute(pa);
    m_dmeConn.saveObject("kmBuildData_jdm", buildData, true);
    // 2. Create & save Mining Function Settings
    // Create kMeans algorithm settings
    OraKMeansSettings kmAlgo =
      (OraKMeansSettings) m_kmeansFactory.create();
    kmAlgo.setDistanceFunction(ClusteringDistanceFunction.euclidean);
    kmAlgo.setMaxNumberOfIterations(10);
    kmAlgo.setMinErrorTolerance(0.01);
    kmAlgo.setSplitCriterion(OraSplitCriterion.clusterVariance);
    kmAlgo.setNumberOfBins(10);
    kmAlgo.setBlockGrowth(2);
    kmAlgo.setMinPercentageAtrrSupport(0.1);

    // 3. Create ClusteringSettings
    ClusteringSettings buildSettings = m_clusFactory.create();
    buildSettings.setAlgorithmSettings(kmAlgo);
    buildSettings.setMaxNumberOfClusters(10);
    ((OraBuildSettings) buildSettings).useAutomatedDataPreparations(true);
    m_dmeConn.saveObject("kmBuildSettings_jdm", buildSettings, true);
    // 4. Create, save & execute Build Task
    BuildTask buildTask = //Build data specification
      //Mining function settings name
      //Mining model name
      m_buildFactory.create("kmBuildData_jdm", "kmBuildSettings_jdm",
                            "kmModel_jdm");
    buildTask.setDescription("kmBuildTask_jdm");
    saveTask(buildTask, "kmBuildTask_jdm", null);

  }

  /**
   *
   *    For a descriptive mining function like Clustering, "Scoring" involves
   *  providing the probability values for each cluster.
   *    After completing the apply task, an apply output table
   * "km_apply_output_jdm" will be created at the user specfied location.
   *
   * @exception JDMException if model apply failed
   */
  public static void applyModel()
    throws JDMException
  {
    //1. Create & save PhysicalDataSpecification
    PhysicalDataSet applyData =
      m_pdsFactory.create("MINING_DATA_APPLY_V", false);
    PhysicalAttribute pa =
      m_paFactory.create("cust_id", AttributeDataType.integerType,
                         PhysicalAttributeRole.caseId);
    applyData.addAttribute(pa);
    m_dmeConn.saveObject("kmApplyData_jdm", applyData, true);
    //2. Create & save ClassificationApplySettings
    ClusteringApplySettings clusAS = m_applySettingsFactory.create();
    m_dmeConn.saveObject("kmApplySettings_jdm", clusAS, true);

    //3. Create, store & execute apply Task
    DataSetApplyTask applyTask =
      m_dsApplyFactory.create("kmApplyData_jdm", "kmModel_jdm",
                              "kmApplySettings_jdm",
                              "km_apply_output_jdm");
    saveTask(applyTask, "kmApplyTask_jdm", "kmBuildTask_jdm");
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
    System.out.print("Waiting for the completion of kmBuildTask_jdm. ");
    ExecutionStatus buildTaskCompletionStatus =
      m_dmeConn.getLastExecutionHandle("kmBuildTask_jdm").waitForCompletion(Integer.MAX_VALUE);
    //If successful
    if (ExecutionState.success.equals(buildTaskCompletionStatus.getState()))
    {
      System.out.println("It is successful. ");
      //Restore the model from the DME and explore the details of the model
      ClusteringModel model =
        (ClusteringModel) m_dmeConn.retrieveObject("kmModel_jdm",
                                                   NamedObject.model);
      displayKMModelDetails(model);
      //2. Monitor applyTask
      //Wait for the completion of the task
      {
        System.out.print("Waiting for the completion of kmApplyTask_jdm. ");
        ExecutionStatus applyTaskCompletionStatus =
          m_dmeConn.getLastExecutionHandle("kmApplyTask_jdm").waitForCompletion(Integer.MAX_VALUE);
        //If successful
        if (ExecutionState.success.equals(applyTaskCompletionStatus.getState()))
        {
          System.out.println("It is successful. ");
          //Display results
          displayScoringResults();
        }
        else
        {
          System.out.println("It is at state:" +
                             applyTaskCompletionStatus.getState().name() +
                             ((applyTaskCompletionStatus.getDescription() ==
                               null)? "":
                              "State Description:" + applyTaskCompletionStatus.getDescription()));
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

  /**
   * Shows scoring results.
   *   BUSINESS CASE 1: List the clusters into which the customers in this
   * dataset have been grouped.
   *   BUSINESS CASE 2: List ten most representative (based on likelihood)
   * customers of cluster 2.
   */
  public static void displayScoringResults()
  {
    // BUSINESS CASE 1: List the clusters into which the customers in this
    // dataset have been grouped.
    String sqlCase1 =
      "SELECT clus, COUNT(*) AS CNT FROM " + " (SELECT cluster_id CLUS, " +
      "   ROW_NUMBER() OVER " +
      "          (PARTITION BY CUST_ID ORDER BY PROBABILITY DESC) CLUS_RNK " +
      " FROM km_apply_output_jdm) " + "WHERE CLUS_RNK = 1 " +
      "GROUP BY CLUS ORDER BY CNT DESC";

    Statement stmt = null;
    ResultSet rs = null;
    java.sql.Connection dbConn =
      ((OraConnection) m_dmeConn).getDatabaseConnection();
    try
    {
      stmt = dbConn.createStatement();
      rs = stmt.executeQuery(sqlCase1);
      System.out.println("Cluster ID Count");
      System.out.println("----------------------------");
      while (rs.next())
      {
        int clus = rs.getInt(1);
        int cnt = rs.getInt(2);
        System.out.println(TAB + clus + TAB + TAB + cnt);
      }
    }
    catch (SQLException anySqlExp)
    {
      System.out.println(anySqlExp);
    }
    finally
    {
      try
      {
        stmt.close();
      }
      catch (Exception anySqlExp)
      {
      }
    }

    // BUSINESS CASE 2: List ten most representative (based on likelihood)
    // customers of cluster 2

    String sqlCase2 =
      "SELECT * from " + " (SELECT cust_id, probability from km_apply_output_jdm " +
      " WHERE CLUSTER_ID = 2 ORDER BY probability DESC) " +
      "WHERE ROWNUM < 11";

    stmt = null;
    rs = null;
    try
    {
      stmt = dbConn.createStatement();
      rs = stmt.executeQuery(sqlCase2);
      System.out.println("Cust_ID          Probability");
      System.out.println("----------------------------");
      while (rs.next())
      {
        int custid = rs.getInt(1);
        String probability = rs.getString(2);
        System.out.println(TAB + custid + TAB + TAB + probability);
      }
    }
    catch (SQLException anySqlExp)
    {
      System.out.println(anySqlExp);
    }
    finally
    {
      try
      {
        stmt.close();
      }
      catch (Exception anySqlExp)
      {
      }
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
    ((OraTask) taskObj).overwriteOutput(true); //Since OJDM 11.1
    m_dmeConn.saveObject(taskName, taskObj, true);
  }

  /**
   * This method displayes KM model details.
   *
   * @param model to be presented
   *
   * @exception JDMException if failed to retrieve model details
   */
  public static void displayKMModelDetails(ClusteringModel model)
    throws JDMException
  {
    String modelName = model.getName();
    System.out.println("Model Name: " + modelName);
    System.out.println("Clustering model details:");
    System.out.println(TAB + "Number of clusters: " +
                       model.getNumberOfClusters());
    System.out.println(TAB + "Number of tree levels: " +
                       model.getNumberOfLevels());
    Collection clusters = model.getClusters();
    if (clusters == null || clusters.isEmpty() == true)
    {
      System.out.println("Unable to retrieve clusters.");
      return;
    }

    // Display root cluster(s)
    Collection vRootClusters = model.getRootClusters();
    Cluster[] rootCluster = null;
    if (vRootClusters != null && vRootClusters.isEmpty() == false)
    {
      rootCluster =
          (Cluster[]) vRootClusters.toArray(new Cluster[vRootClusters.size()]);
      if (rootCluster != null)
        System.out.println(TAB + "Root Cluster Id: " +
                           rootCluster[0].getClusterId());
    }

    // Display leaf clusters
    Collection vLeafClusters = model.getLeafClusters();
    if (vLeafClusters != null && vLeafClusters.isEmpty() == false)
    {
      Cluster[] leafClusters =
        (Cluster[]) vLeafClusters.toArray(new Cluster[vLeafClusters.size()]);
      System.out.println(UNDERLINE);
      System.out.println(LEAF_CLUSTERS_HEADER);
      System.out.println(UNDERLINE);
      for (int ni = 0; ni < leafClusters.length; ni++)
      {
        printSingleClusterDetails(leafClusters[ni]);
        if (ni == 0)
        {
          // display first leaf cluster statistics
          printClusterStatistics(model, leafClusters[ni], 1);
        }
      }
    }

    // Display all model rules
    Collection vRules = model.getRules();
    if (vRules != null && vRules.isEmpty() == false)
    {
      Rule[] rules = (Rule[]) vRules.toArray(new Rule[vRules.size()]);
      System.out.println(CR + CR + UNDERLINE);
      System.out.println(RULES_CLUSTERS_HEADER);
      System.out.println(UNDERLINE);
      for (int rl = 0; rl < rules.length; rl++)
      {
        printRuleNoDetails(rules[rl], 0);
      }
    }

    if (rootCluster != null)
    {
      // print hierarchy
      System.out.println(CR + CR + UNDERLINE);
      System.out.println(RULES_CLUSTERS_HIERARCHY_HEADER);
      System.out.println(UNDERLINE);
      // 1.print root
      printRootClusterDetails(rootCluster[0]);
      // 1.print children
      Cluster[] children = rootCluster[0].getChildren();
      int indent = 0;
      if (children != null)
      {
        for (int k = 0; k < children.length; k++)
        {
          printRecursiveClusterDetails(model, children[k], indent);
        }
      }
      return;
    }

    // print flat structure
    Cluster[] arrayClusters =
      (Cluster[]) clusters.toArray(new Cluster[clusters.size()]);
    for (int ni = 0; ni < arrayClusters.length; ni++)
    {
      printSingleClusterDetails(arrayClusters[ni]);
    }
    System.out.println(CR + UNDERLINE + CR);
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

  /**
   * This method recursively displayes Cluster details.
   *
   * @param model model to be presented
   * @param cluster current cluster which details are being presented
   * @param indent indentation level to illustrate cluster parent-child relation
   *
   * @exception JDMException if failed to retrieve cluster details
   */
  public static void printRecursiveClusterDetails(ClusteringModel model,
                                                  Cluster cluster,
                                                  int indent)
    throws JDMException
  {
    indent++;
    System.out.println(getIndentation(indent,
                                      "Cluster Id: " + cluster.getClusterId()) +
                       CR +
                       getIndentation(indent, "Case Count: " + cluster.getCaseCount()) +
                       CR +
                       getIndentation(indent, "Tree Level: " + cluster.getLevel()) +
                       CR +
                       getIndentation(indent, "Dispersion: " + ((OraCluster) cluster).getDispersion()) +
                       CR +
                       getIndentation(indent, "Parent's id: " + (cluster.getParent() !=
                                                                 null?
                                                                 String.valueOf(cluster.getParent().getClusterId()):
                                                                 "")));

    Cluster[] ancestors = cluster.getAncestors();
    if (ancestors != null)
    {
      StringBuffer sbTab = new StringBuffer("Anchestors");
      if (ancestors.length == 0)
        sbTab.append(" : None");
      for (int j = 0; j < ancestors.length; j++)
      {
        for (int sp = 0; sp < j; sp++)
          sbTab.append(":" + ancestors[j].getClusterId() + " ");
      }
      System.out.print(getIndentation(indent, sbTab.toString()));
    }

    Cluster[] children = cluster.getChildren();
    if (children != null)
    {
      System.out.println(CR + getIndentation(indent, "Children:"));
      for (int k = 0; k < children.length; k++)
      {
        Cluster childCluster = children[k];
        printRecursiveClusterDetails(model, childCluster, indent);
      }
    }
    else
    {
      System.out.print(CR +
                       getIndentation(indent, "No child clusters" + CR));
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
  public static void printRuleDetails(Rule rule, int indent)
  {
    System.out.println(CR_TAB +
                       getIndentation(indent, "Rule number:" + rule.getRuleIdentifier()) +
                       CR_TAB + TAB +
                       getIndentation(indent, "Support: " + rule.getSupport()) +
                       CR_TAB + TAB +
                       getIndentation(indent, "Confidence: " +
                                      rule.getConfidence()));
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
   * This method prints Rules details without antecedent and consequent information.
   *
   * @param rule rule which details are being presented
   * @param indent indentation level to illustrate cluster parent-child relation
   */
  public static void printRuleNoDetails(Rule rule, int indent)
  {
    System.out.println(CR_TAB +
                       getIndentation(indent, "Rule number:" + rule.getRuleIdentifier()) +
                       CR_TAB + TAB +
                       getIndentation(indent, "Support: " + rule.getSupport()) +
                       CR_TAB + TAB +
                       getIndentation(indent, "Confidence: " +
                                      rule.getConfidence()));
  }

  /**
   * This method prints antecedent information.
   *
   * @param predicate antecedent being presented
   * @param indent indentation level to illustrate cluster parent-child relation
   *
   * @exception JDMException if failed to retrieve antecedent details
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

      // combine predicates by attribute name
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
   * This method prints consequent information.
   *
   * @param predicate consequent being presented
   * @param indent indentation level to illustrate cluster parent-child relation
   *
   * @exception JDMException if failed to retrieve consequent details
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
   * This method prints <code>SimplePredicate</code> information.
   *
   * @param predicate <code>SimplePredicate</code> being presented
   *
   * @exception JDMException if failed to retrieve <code>SimplePredicate</code> details
   */
  public static StringBuffer printSimplePredicate(SimplePredicate predicate)
    throws JDMException
  {
    StringBuffer sb = new StringBuffer();
    if (predicate.isNumericalValue())
    {
      //((OraSimplePredicate)predicate).getComparisonOperatorValue() + " " +
      sb.append(predicate.getAttributeName() + " " +
                OraPLSQLMappings.getComparisonOperatorValue_tree(((OraSimplePredicate) predicate).getComparisonOperator()) +
                " " + predicate.getNumericalValue());
    }
    else
    {
      //((OraSimplePredicate)predicate).getComparisonOperatorValue() + " "
      sb =
          new StringBuffer(predicate.getAttributeName() + " " + OraPLSQLMappings.getComparisonOperatorValue_tree(((OraSimplePredicate) predicate).getComparisonOperator()) +
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
   * This method shows details of the root cluster.
   *
   * @param cluster root cluster
   *
   * @exception JDMException if failed to retrieve root cluster details
   */
  public static void printRootClusterDetails(Cluster cluster)
    throws JDMException
  {
    System.out.println(CR + "Root Cluster Id: " + cluster.getClusterId() +
                       CR_TAB + "Case Count: " + cluster.getCaseCount() +
                       CR_TAB + "Tree Level: " + cluster.getLevel() +
                       CR_TAB + "Dispersion: " +
                       ((OraCluster) cluster).getDispersion() + CR_TAB +
                       "Children:");
  }

  /**
   * This method shows details of any cluster.
   *
   * @param cluster clusterbeing presented
   *
   * @exception JDMException if failed to retrieve this cluster details
   */
  public static void printSingleClusterDetails(Cluster cluster)
    throws JDMException
  {
    System.out.println(CR + "Cluster Id: " + cluster.getClusterId() +
                       CR_TAB + "Case Count: " + cluster.getCaseCount() +
                       CR_TAB + "Tree Level: " + cluster.getLevel() +
                       CR_TAB + "Dispersion: " +
                       ((OraCluster) cluster).getDispersion() + CR_TAB +
                       "Parent's id: " +
                       (cluster.getParent() != null? String.valueOf(cluster.getParent().getClusterId()):
                        "") + CR_TAB + "Is root Cluster: " +
                       cluster.isRoot() + CR_TAB + "Is leaf Cluster: " +
                       cluster.isLeaf());

    Cluster[] ancestors = cluster.getAncestors();
    StringBuffer sbTab = new StringBuffer(TAB + "Anchestors ");
    if (ancestors != null)
    {
      for (int j = 0; j < ancestors.length; j++)
      {
        for (int sp = 0; sp < j; sp++)
          sbTab.append(":" + ancestors[j].getClusterId() + " ");
      }
    }
    else
    {
      sbTab.append("None");
    }
    System.out.print(sbTab.toString());

    Cluster[] children = cluster.getChildren();
    System.out.println(CR_TAB + "Children:");
    if (children != null)
    {
      for (int k = 0; k < children.length; k++)
      {
        Cluster childCluster = children[k];
        System.out.print(TAB + TAB + "Child: " +
                         childCluster.getClusterId() + CR);
      }
      System.out.println(CR);
    }
    else
    {
      System.out.print(TAB + TAB + "None");
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
  public static void printClusterStatistics(ClusteringModel model,
                                            Cluster cluster, int indent)
  {
    try
    {
      ModelSignature modelSignature = model.getSignature();

      AttributeStatisticsSet attrStat = cluster.getStatistics();
      if (attrStat == null)
        return;
      TreeMap centroids = ((OraCluster) cluster).getCentroids();
      if (centroids == null || centroids.isEmpty())
      {
        System.out.println("Error: cluster " + cluster.getClusterId() +
                           " does not contain statsitics");
        return;
      }
      Set centroidKeys = centroids.keySet();
      Iterator attributesNames = centroidKeys.iterator();
      while (attributesNames.hasNext())
      {
        String attributeName = (String) attributesNames.next();
        SignatureAttribute sa = modelSignature.getAttribute(attributeName);
        AttributeType aType = sa.getAttributeType();
        UnivariateStatistics stats = attrStat.getStatistics(attributeName);
        double[] frequencies = stats.getProbabilities();
        System.out.println(CR_TAB +
                           getIndentation(indent, "Statistics for attribute: " +
                                          attributeName));
        System.out.println(TAB + getIndentation(indent, UNDERLINE));
        if (aType.equals(AttributeType.numerical))
        {
          Interval[] ranges = (Interval[]) stats.getValues();
          if (ranges != null)
          {
            System.out.println(TAB + TAB +
                               getIndentation(indent, "Bin Id" + TAB +
                                              TAB + TAB + "Range" + TAB +
                                              TAB + TAB + TAB +
                                              "Frequency"));
            for (int in = 0; in < ranges.length; in++)
            {
              Interval bin = ranges[in];
              IntervalClosure closure = bin.getIntervalClosure();
              String openParenthesis =
                ((closure.equals(IntervalClosure.openClosed) ||
                  closure.equals(IntervalClosure.openOpen))? "( ": "[ ");
              String closeParenthesis =
                ((closure.equals(IntervalClosure.openOpen) ||
                  closure.equals(IntervalClosure.closedOpen))? " )":
                 " ]"); // bin Id
              // range
              //frequency
              System.out.println(TAB + TAB +
                                 getIndentation(indent, String.valueOf(in +
                                                                       1)) +
                                 TAB + TAB +
                                 getIndentation(indent, openParenthesis +
                                                bin.getStartPoint() +
                                                " - " + bin.getEndPoint() +
                                                closeParenthesis) + TAB +
                                 TAB +
                                 getIndentation(indent, String.valueOf((int) frequencies[in])));
            }
          }
        }
        else if (aType.equals(AttributeType.categorical))
        {
          Object[] values = stats.getValues();
          if (values != null)
          {
            System.out.println(TAB + TAB +
                               getIndentation(indent, "Bin Id" + TAB +
                                              "Category" + TAB +
                                              "Frequency"));
            for (int in = 0; in < values.length; in++)
            {
              // bin Id
              //value
              //frequency
              System.out.println(TAB + TAB +
                                 getIndentation(indent, String.valueOf(in +
                                                                       1)) +
                                 TAB + TAB + values[in] + TAB + TAB +
                                 getIndentation(indent,
                                                String.valueOf((int) frequencies[in])));
            }
          }
        }
      }
    }
    catch (Exception e)
    {
      System.out.println("Error printing statsitics for cluster: " +
                         cluster.getClusterId());
    }
  }

  private static void clean()
  {
    //Drop the model
    try
    {
      m_dmeConn.removeObject("kmModel_jdm", NamedObject.model);
    }
    catch (JDMException jdmExp)
    {
    }        
  }
}

