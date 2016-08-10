// Copyright (c) 2004, 2006, Oracle. All rights reserved.
// File: dmardemo.java
import java.math.BigDecimal;

import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.ResultSetMetaData;

import java.sql.SQLException;

import java.sql.Statement;

import java.text.DecimalFormat;

import java.util.Collection;

import java.util.Iterator;

import java.util.TreeSet;

import javax.datamining.ExecutionHandle;
import javax.datamining.ExecutionState;
import javax.datamining.ExecutionStatus;
import javax.datamining.JDMException;
import javax.datamining.MiningAlgorithm;
import javax.datamining.MiningFunction;
import javax.datamining.NamedObject;
import javax.datamining.SortOrder;
import javax.datamining.association.AssociationModel;
import javax.datamining.association.AssociationSettings;
import javax.datamining.association.AssociationSettingsFactory;
import javax.datamining.association.Itemset;
import javax.datamining.association.RuleProperty;
import javax.datamining.association.RulesFilterFactory;
import javax.datamining.base.AlgorithmSettings;
import javax.datamining.base.Task;
import javax.datamining.data.AttributeDataType;
import javax.datamining.data.PhysicalAttribute;
import javax.datamining.data.PhysicalAttributeFactory;
import javax.datamining.data.PhysicalAttributeRole;
import javax.datamining.data.PhysicalDataSet;
import javax.datamining.data.PhysicalDataSetFactory;
import javax.datamining.resource.Connection;
import javax.datamining.resource.ConnectionFactory;
import javax.datamining.resource.ConnectionSpec;

import javax.datamining.task.BuildTask;

import javax.datamining.task.BuildTaskFactory;

import oracle.dmt.jdm.association.OraAssociationRule;
import oracle.dmt.jdm.association.OraItemset;
import oracle.dmt.jdm.association.OraRulesFilter;
import oracle.dmt.jdm.resource.OraConnection;
import oracle.dmt.jdm.resource.OraConnectionFactory;

/**
*   This demo program describes how to use the Oracle Data Mining (ODM) Java API
* to build an Association model by using Apriori (AR) algorithm.
* ------------------------------------------------------------------------------
*                             PROBLEM DEFINITION
* ------------------------------------------------------------------------------
*   Find the associations between items bought by customers.
* ------------------------------------------------------------------------------
*                             DATA DESCRIPTION
* ------------------------------------------------------------------------------
*   Data for this demo is composed from a small subset of sales transactions
* from base tables in the Sales History (SH) Schema. The SH schema is an Oracle Database
* Sample Schema that has customer demographics, purchasing, and
* response details for the previous affinity card programs. Data
* exploration and preparing the data is a common step before doing data mining.
* Here in this demo, the following views are created in the user schema using
* SALES and PRODUCT tables.
*
* SALES_TRANS_CUST_V:
*   This view collects the previous customers' purchasing details for building the
* model.  This view will contain purchasing details for customers who have a
* cust_id between 100001 AND 104500.
* ------------------------------------------------------------------------------
*                             DATA MINING PROCESS
* ------------------------------------------------------------------------------
*   Prepare Data:
*   1. Common item removal: common items are candidates for removal during model
*   build, because if a majority of customers have bought those items, the
*   resulting rules do not have much value.  Since the dataset is small, we will
*   skip common item removal.
*
*   2. Data format transformation: market basket or sales datasets are
*   transactional in nature, and form dimension tables in a typical DW. We
*   present such transactional data to the API using an Object View that
*   contains a nested table column holding a collection of items that correspond
*   to a given case id.
*
*   3. Binning transformation: it is used to reduce the cardinality of higher
*   cardinality attributes. The prepareData() method in this demo program
*   illustrates the preparation of the build data.  We skip binning
*   transformation in this demo.  See dmabdemo.java for an example of quantile
*   binning.
*
*   Build Model:
*     Mining Model is the prime object in data mining. The buildModel() method
*   illustrates how to build an Association model by using the Apriori algorithm.
*
*   Test Model:
*     Association rules do not have a predefined test metric.  Two indirect
*   measures of modeling success are:
*
*    1. Number of Rules generated: The optimal number of rules is application
*    dependent. In general, an overwhelming number of rules is undesirable for
*    user interpretation. More rules take longer to compute, and also consume
*    storage and CPU cycles.  You should avoid too many rules by increasing the
*    value for support.
*
*    2. Relevance of rules:  This can be determined only by
*    user inspection of rules, since it is application dependent. Ideally, we
*    want to find rules with high confidence and with non-obvious patterns. The
*    value for confidence is an indicator of the strength of the rule - so you
*    could set the confidence value high in conjunction with support and see if
*    you get high quality rules.
*
*    3. Frequent itemsets provide an insight into co-occurrence of items.
* ------------------------------------------------------------------------------
*                             EXECUTING DEMO PROGRAM
* ------------------------------------------------------------------------------
*   Refer to Oracle Data Mining Administrator's Guide
*   for guidelines for executing this demo program.
*/
// Generic Java api imports
// Java Data Mining (JDM) standard api imports
// Oracle Java Data Mining (JDM) implemented api imports
/**
*   This demo program describes how to use the Oracle Data Mining (ODM) Java API
* to build an Association model by using Apriori (AR) algorithm.
* ------------------------------------------------------------------------------
*                             PROBLEM DEFINITION
* ------------------------------------------------------------------------------
*   Find the associations between items bought by customers.
* ------------------------------------------------------------------------------
*                             DATA DESCRIPTION
* ------------------------------------------------------------------------------
*   Data for this demo is composed from a small subset of sales transactions
* from base tables in the Sales History (SH) Schema. The SH schema is an Oracle Database
* Sample Schema that has customer demographics, purchasing, and
* response details for the previous affinity card programs. Data
* exploration and preparing the data is a common step before doing data mining.
* Here in this demo, the following views are created in the user schema using
* SALES and PRODUCT tables.
*
* SALES_TRANS_CUST_V:
*   This view collects the previous customers' purchasing details for building the
* model.  This view will contain purchasing details for customers who have a
* cust_id between 100001 AND 104500.
* ------------------------------------------------------------------------------
*                             DATA MINING PROCESS
* ------------------------------------------------------------------------------
*   Create input view:
*     In this demo we illustrate creation of a nested table based view, that 
*   has sales transaction data as a nested structure for each customer. This
*   view is used as input for AR model build.
*
*   Build Model:
*     Mining Model is the prime object in data mining. The buildModel() method
*   illustrates how to build an Association model by using the Apriori algorithm.
*
*   Test Model:
*     Association rules do not have a predefined test metric.  Two indirect
*   measures of modeling success are:
*
*    1. Number of Rules generated: The optimal number of rules is application
*    dependent. In general, an overwhelming number of rules is undesirable for
*    user interpretation. More rules take longer to compute, and also consume
*    storage and CPU cycles.  You should avoid too many rules by increasing the
*    value for support.
*
*    2. Relevance of rules:  This can be determined only by
*    user inspection of rules, since it is application dependent. Ideally, we
*    want to find rules with high confidence and with non-obvious patterns. The
*    value for confidence is an indicator of the strength of the rule - so you
*    could set the confidence value high in conjunction with support and see if
*    you get high quality rules.
*
*    3. Frequent itemsets provide an insight into co-occurrence of items.
* ------------------------------------------------------------------------------
*                             EXECUTING DEMO PROGRAM
* ------------------------------------------------------------------------------
*   Refer to Oracle Data Mining Administrator's Guide
*   for guidelines for executing this demo program.
*/
public class

dmardemo
  extends Object
{
  //Connection related data members
  private static Connection m_dmeConn;
  private static ConnectionFactory m_dmeConnFactory;
  //Object factories used in this demo program
  private static PhysicalDataSetFactory m_pdsFactory;
  private static PhysicalAttributeFactory m_paFactory;
  private static AssociationSettingsFactory m_assoFactory;
  private static RulesFilterFactory m_filterFactory;
  private static BuildTaskFactory m_buildFactory;
  private static String m_pdsName = "arBuildData_jdm";
  private static String m_settingsName = "arSettings_jdm";
  private static String m_modelName = "arModel_jdm";
  private static String m_taskName = "arBuildTask_jdm";
  // Global constants
  private static DecimalFormat m_df = new DecimalFormat("##.####");
  // Default settings
  private static int m_maxRuleLength = 20;
  private static double m_minSupport = 0.1;
  private static double m_minConfidence = 0.1;

  public static void main(String[] args)
  {
    try
    {
      if ((args.length != 0) & (args.length != 3))
      {
        System.out.println("Usage: java dmardemo ");
        System.out.println("   or: java dmardemo <Host name>:<Port>:<SID> <User Name> <Password>");
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
      // 4. Create input view
      createInputView();
      // 5. Build a model
      buildModel();
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
    m_assoFactory =
        (AssociationSettingsFactory) m_dmeConn.getFactory("javax.datamining.association.AssociationSettings");
    m_buildFactory =
        (BuildTaskFactory) m_dmeConn.getFactory("javax.datamining.task.BuildTask");
    m_filterFactory =
        (RulesFilterFactory) m_dmeConn.getFactory("javax.datamining.association.RulesFilter");
  }

  /**
   * This method illustrates preparation of the data for build operations by
   * using Common item removal, Data format transformation, and binning
   * transformations.
   *
   * Transformations:
   * 1. Common item removal: Skipped since the dataset is small.
   *
   * 2. Column format transformation: Converted a table column into a nested
   * table column format, so each record contains a nested table column
   * holding a collection of items that correspond to a given customer id.
   *
   * The following table illustrates customer attributes available for building
   * the mining model and data characteristics.
   *
   * COLUMN_NAME           DATA_TYPE    DISTINCT  NUMBER OF ROWS
   * ------------------    ---------    --------  --------------
   * CUST_ID               NUMBER       940       2804
   * PROD_NAME             VARCHAR2(50) 14        2804
   * HAS_IT                NUMBER       1         2804
   *
   * Unprepared Data       --->  Prepared(nested table column) Data
   * --------------------        ----------------------------------
   * SALES_TRANS_CUST_V          SALES_TRANS_CUST_AR_V
   */
  public static void createInputView()
    throws JDMException
  {
    System.out.println("--- Create AR model build input view 'SALES_TRANS_CUST_V'  ---");
    boolean isOutputAsView = false;
    String inputDataURI = null;
    String outputDataURI = null;

    // 1. Create the database view in a transactional format
    //    that joins customers sales transactions table SH.SALES
    //    with the products table SH.PRODUCTS that contains
    //    the product name. In this example 4500 customers transactions
    //    are taken as input.
    isOutputAsView = true;
    inputDataURI = "SH.SALES a, SH.PRODUCTS b";
    outputDataURI = "SALES_TRANS_CUST_V";
    String createView =
      "CREATE VIEW SALES_TRANS_CUST_V AS " + " SELECT cust_id, prod_name, 1 has_it " +
      " FROM (SELECT a.cust_id, b.prod_name " +
      "       FROM SH.SALES a, SH.PRODUCTS b " +
      "       WHERE a.prod_id = b.prod_id AND " +
      "             a.cust_id between 100001 AND 104500) " +
      " GROUP BY cust_id, prod_name ";

    // Execute the sql statement
    java.sql.Connection dbConn =
      ((OraConnection) m_dmeConn).getDatabaseConnection();
    PreparedStatement pStmt = null;
    try
    {
      pStmt = dbConn.prepareStatement(createView);
      pStmt.executeQuery();
    }
    catch (Exception anySqlExp)
    {
      anySqlExp.printStackTrace(); // Ignore
    }
    finally
    {
      try
      {
        pStmt.close();
      }
      catch (Exception ex)
      {
      }
      ; // Ignore
    }

    // 2. Convert the transactional view created in the previous step
    //    "SALES_TRANS_CUST_V" to a nested table based view that
    //    represents the transactions by a customer is stored as a
    //    nested table, in this new view there will be one record per
    //    customer in the main view and this record contains the
    //    products purchased as a nested view
    isOutputAsView = true;
    inputDataURI = "SALES_TRANS_CUST_V";
    outputDataURI = "SALES_TRANS_CUST_AR_V";
    String caseId = "CUST_ID";
    String originalColumn = "PROD_NAME";
    String newNestedColumn = "CUSTPRODS";
    String createNestedColumn =
      "CREATE VIEW SALES_TRANS_CUST_AR_V AS " + "SELECT CUST_ID, " +
      "CAST(COLLECT( " +
      "   DM_Nested_Numerical(SUBSTR(PROD_NAME, 1, 30), has_it) " +
      "  ) AS DM_Nested_Numericals) CUSTPRODS " +
      "FROM SALES_TRANS_CUST_V " + "GROUP BY CUST_ID";

    // Execute the sql statement
    try
    {
      pStmt = dbConn.prepareStatement(createNestedColumn);
      pStmt.executeQuery();
    }
    catch (Exception anySqlExp)
    {
      anySqlExp.printStackTrace(); // Ignore
    }
    finally
    {
      try
      {
        pStmt.close();
      }
      catch (Exception ex)
      {
      }
      ; // Ignore
    }
  }

  /**
   * This method illustrates how to build an association mining model
   * using SALES_TRANS_CUST_AR_V dataset and mining function settings.
   */
  public static void buildModel()
    throws JDMException
  {
    System.out.println("---------------------------------------------------");
    System.out.println("--- Build Model                                 ---");
    System.out.println("---------------------------------------------------");
    // 1. Create & save PhysicalDataSpecification
    PhysicalDataSet buildData =
      m_pdsFactory.create("SALES_TRANS_CUST_AR_V", false);
    PhysicalAttribute pa =
      m_paFactory.create("CUST_ID", AttributeDataType.integerType,
                         PhysicalAttributeRole.caseId);
    buildData.addAttribute(pa);
    m_dmeConn.saveObject(m_pdsName, buildData, false);
    // 2. Create & save Mining Function Settings
    // Create AssociationSettings
    AssociationSettings buildSettings = m_assoFactory.create();
    //
    //   Association Rules in ODM works best on sparse data - i.e. data where
    // the average number of attributes/items associated with a given case is
    // a small percentage of the total number of possible attributes/items.
    // This is true of most market basket datasets where an average customer
    // purchases only a small subset of items from a fairly large inventory
    // in the store.
    //
    //   In this demo data set, we will compute density of data and
    // average number of products purchased per customer, to determine
    // the algorithm settings value.
    //
    // Compute the density of data
    //       COUNT(*) / COUNT(DISTINCT CUST_ID) * COUNT(DISTINCT PROD_NAME)
    //           2804 / 940 * 14 --> 2.2 (22%)
    //
    // Compute the average number of products purchased per customer
    //    AVG (SELECT COUNT(PROD_NAME)
    //         FROM   SALES_TRANS_CUST_AR_V
    //         GROUP BY CUST_ID) --> 2.98 (3 items)
    //
    //   For sparse data, it is common to have a density below 1%, and AR
    // typically performs well with sparse data. The above dataset,
    // SALES_TRANS_CUST_AR_V, is moderately dense, 22%, (because of the
    // aggregation), and we have to tune the AR parameters in accordance.
    //
    // Start with
    //
    m_minSupport = 10.0f; // 10%
    m_minConfidence = 10.0f; // 10%
    // Since the average number of products purchased per customer is 2.98, so
    // we will set Max Rule Length (i.e. Items) = 3
    m_maxRuleLength = 3;
    buildSettings.setMinSupport(m_minSupport);
    buildSettings.setMinConfidence(m_minConfidence);
    buildSettings.setMaxRuleLength(m_maxRuleLength);
    m_dmeConn.saveObject(m_settingsName, buildSettings, true);
    // 3. Create, save & execute Build Task
    BuildTask buildTask = //Build data specification
      //Mining function settings name
      //Mining model name
      m_buildFactory.create(m_pdsName, m_settingsName, m_modelName);
    buildTask.setDescription(m_taskName);
    executeTask(buildTask, m_taskName);
    // 4. Restore the model from the data mining server
    AssociationModel model =
      (AssociationModel) m_dmeConn.retrieveObject(m_modelName,
                                                  NamedObject.model);
    // 5. Explore the details of the restored model
    // Display model build settings
    AssociationSettings retrievedBuildSettings =
      (AssociationSettings) model.getBuildSettings();
    if (buildSettings == null)
      System.out.println("Failure to restore build settings.");
    else
      displayBuildSettings(retrievedBuildSettings, m_settingsName);
    // Display the rules
    displayAssociationRules(model);
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
    throws JDMException
  {
    boolean isTaskSuccess = false;
    m_dmeConn.saveObject(taskName, taskObj, true);
    ExecutionHandle execHandle = m_dmeConn.execute(taskName);
    System.out.print(taskName + " is started, please wait. ");
    //Wait for completion of the task
    ExecutionStatus status =
      execHandle.waitForCompletion(Integer.MAX_VALUE);
    //Check the status of the task after completion
    isTaskSuccess = status.getState().equals(ExecutionState.success);
    if (isTaskSuccess)
    {
      //Task completed successfully
      System.out.println(taskName + " is successful.");
    }
    else
    { //Task failed
      System.out.println(taskName + " is failed.\nFailure Description: " +
                         status.getDescription());
    }
    return isTaskSuccess;
  }

  private static void displayBuildSettings(AssociationSettings assocSettings,
                                           String buildSettingsName)
  {
    System.out.println("BuildSettings Details from the " +
                       buildSettingsName + " table:");
    displayTable(buildSettingsName, "", "order by SETTING_NAME");
    System.out.println("BuildSettings Details from the " +
                       buildSettingsName +
                       " model build settings object:");
    String objName = assocSettings.getName();
    if (objName != null)
      System.out.println("Name = " + objName);
    String objDescription = assocSettings.getDescription();
    if (objDescription != null)
      System.out.println("Description = " + objDescription);
    java.util.Date creationDate = assocSettings.getCreationDate();
    String creator = assocSettings.getCreatorInfo();
    AlgorithmSettings algoSettings = assocSettings.getAlgorithmSettings();
    if (algoSettings == null)
      System.out.println("Failure: assocSettings.getAlgorithmSettings() returns null");
    MiningAlgorithm algo = algoSettings.getMiningAlgorithm();
    if (algo != null)
      System.out.println("Algorithm Name: " + algo.name());
    MiningFunction function = assocSettings.getMiningFunction();
    if (function == null)
      System.out.println("Failure: assocSettings.getMiningFunction() returns null");
    System.out.println("Function Name: " + function.name());
    // List of association settings
    int intValue = assocSettings.getMaxRuleLength();
    System.out.println("Max Number of Rules: " + intValue);
    Double doubleValue = assocSettings.getMinConfidence();
    System.out.println("Min Confidence: " +
                       m_df.format(doubleValue.doubleValue()));
    doubleValue = assocSettings.getMinSupport();
    System.out.println("Min Support: " +
                       m_df.format(doubleValue.doubleValue()));
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
            {
              colContent = obj.toString();
              try
              {
                Double testDouble = new Double(colContent);
                colContent = m_df.format(testDouble);
              }
              catch (Exception e)
              {
              }
            }
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
    finally
    {
      try
      {
        rs.close();
        pStmt.close();
      }
      catch (Exception ex)
      {
      }
      ; // Ignore
    }
  }

  /**
   * Displays rules from association model.
   */
  private static void displayAssociationRules(AssociationModel model)
    throws JDMException
  {
    String modelName = model.getName();
    System.out.println("Model Name: " + modelName);
    // DISPLAY TOP-10 FREQUENT ITEMSETS
    //
    System.out.println("================================");
    System.out.println("Display Top-10 frequent itemsets");
    System.out.println("================================");
    // 1. Retrieve item sets collection that is already sorted internally:
    //    Order by support desc,
    //             itemset_id asc,
    //             column_value asc,
    //             number_of_items asc
    Collection itemSets = model.getItemsets();
    Iterator iItemSets = itemSets.iterator();
    // 2. Display the top 10 item sets
    int count = 0;
    while (iItemSets.hasNext())
    {
      if (count > 10)
        break;
      else
        count = count + 1;

      OraItemset itemSet = (OraItemset) iItemSets.next();
      Object[] items = itemSet.getItems();
      String itemList = "";
      for (int i = 0; i < items.length; i++)
        itemList = items[i] + ",";
      System.out.println(itemList + " (support=" +
                         m_df.format(itemSet.getSupport()) +
                         ", number of items=" + itemSet.getSize() + ")");
    }

    // DISPLAY TOP-10 ASSOCIATION RULES
    //
    System.out.println("================================");
    System.out.println("Display Top-10 association rules");
    System.out.println("================================");
    // 1. Set order by confidence DESC, support DESC
    OraRulesFilter filter = (OraRulesFilter) m_filterFactory.create();
    filter.setOrderingCondition(new RuleProperty[]
        { RuleProperty.confidence, RuleProperty.support }, new SortOrder[]
        { SortOrder.descending, SortOrder.descending });
    // 2. Set to return the first 10 rules only
    filter.setMaxNumberOfRules(10);
    // 3. Retrieve rules
    Collection rules = model.getRules(filter);
    Iterator iRules = rules.iterator();
    while (iRules.hasNext())
    {
      OraAssociationRule rule = (OraAssociationRule) iRules.next();
      Itemset antecedent = rule.getAntecedent();
      Object[] ante_items = antecedent.getItems();
      // sort the items in antecedent to produce deterministic order of items
      TreeSet sortedSet = new TreeSet();
      for (int i = 0; i < ante_items.length; i++)
        sortedSet.add(ante_items[i]);
      Iterator sortedI = sortedSet.iterator();
      while (sortedI.hasNext())
        System.out.print(sortedI.next() + " ");
      Itemset consequent = rule.getConsequent();
      Object[] cons_items = consequent.getItems();
      System.out.println("==> " + cons_items[0] + " (support=" +
                         m_df.format(rule.getSupport()) + ", confidence=" +
                         m_df.format(rule.getConfidence()) + ")");
    }
  }

  public static void clean()
  {
    java.sql.Connection dbConn =
      ((OraConnection) m_dmeConn).getDatabaseConnection();
    Statement stmt = null;
    // Drop prepared views
    try
    {
      stmt = dbConn.createStatement();
      stmt.executeUpdate("DROP VIEW SALES_TRANS_CUST_V");
    }
    catch (SQLException anySqlExp)
    {
    } // Ignore
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
    // Drop prepared views
    try
    {
      stmt = dbConn.createStatement();
      stmt.executeUpdate("DROP VIEW SALES_TRANS_CUST_AR_V");
    }
    catch (SQLException anySqlExp)
    {
    } // Ignore
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
    //Drop the model
    try
    {
      m_dmeConn.removeObject(m_modelName, NamedObject.model);
    }
    catch (JDMException jdmExp)
    {
    }
    // drop the build settings
    try
    {
      m_dmeConn.removeObject(m_settingsName, NamedObject.buildSettings);
    }
    catch (JDMException jdmExp)
    {
    }
    // drop the PDS
    try
    {
      m_dmeConn.removeObject(m_pdsName, NamedObject.physicalDataSet);
    }
    catch (JDMException jdmExp)
    {
    }
    // drop the build task
    try
    {
      m_dmeConn.removeObject(m_taskName, NamedObject.task);
    }
    catch (JDMException jdmExp)
    {
    }
  }
}
