// Copyright (c) 2001, 2007, Oracle. All rights reserved.  
// File: dmxfdemo.java

/**
* This demo program describes how to use the Oracle Data Mining (ODM) Java API 
* to perform mining transformations: discretization, clipping, and normalization.
* ------------------------------------------------------------------------------
*                             PROBLEM DEFINITION
* ------------------------------------------------------------------------------
*   Data preparation is an important step preceding actual data mining. The following 
* types of transformation are demonstrated: discretization, clipping and
* normalization. 
*   Discretization involves mapping both continuous and discrete values to discrete 
* values of reduced cardinality. It can be performed on both categorical and 
* numerical attributes. The following types of discretization are demonstrated
* for numerical attributes: equal width, quantile and custom. Top-N method can be
* applied to categorical attributes.
*   Normalization involves scaling continuous values down to specific range - as 
* in -1.0 to 1.0 or 0.0 to 1.0 such that xnew = (xold - shift)/scale. 
* It applies only to numerical attributes. Min-Max Normalization and Z-Score 
* Normalization are shown.
*   Clipping involves setting the tail values of a particular attribute to some 
* specified quantile of the data, or removing the tails. It applies only to 
* numerical attributes. Winsorizing and trimming methods are presented.
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
* MINING_DATA_BUILD_V:
*   This view collects the previous customers' demographic, purchasing, and affinity 
*   card response details for building the model.
* ------------------------------------------------------------------------------
*                             EXECUTING DEMO PROGRAM
* ------------------------------------------------------------------------------
*   Refer to Oracle Data Mining Administrator's Guide
*   for guidelines for executing this demo program.
*/

// Generic api imports
import java.math.BigDecimal;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Statement;
import java.text.DecimalFormat;
import java.text.MessageFormat;
import java.util.Vector;
import java.util.ArrayList;
// Java Data Mining (JDM) standard imports
import javax.datamining.ExecutionHandle;
import javax.datamining.ExecutionState;
import javax.datamining.ExecutionStatus;
import javax.datamining.JDMException;
import javax.datamining.base.Task;
import javax.datamining.resource.ConnectionSpec;
// Oracle Java Data Mining (JDM) implemented api imports
import oracle.dmt.jdm.resource.OraConnection;
import oracle.dmt.jdm.resource.OraConnectionFactory;
import oracle.dmt.jdm.task.OraTransformationTask;
import oracle.dmt.jdm.task.OraTransformationTaskFactory;
import oracle.dmt.jdm.transform.OraTransformationFactory;
import oracle.dmt.jdm.transform.OraTransformationSequence;
import oracle.dmt.jdm.transform.binning.OraAttributeBins;
import oracle.dmt.jdm.transform.binning.OraBinningTransform;
import oracle.dmt.jdm.transform.binning.OraBinningTransformFactory;
import oracle.dmt.jdm.transform.binning.OraCategoricalAttributeBins;
import oracle.dmt.jdm.transform.binning.OraCategoricalBin;
import oracle.dmt.jdm.transform.binning.OraCategoricalBinningType;
import oracle.dmt.jdm.transform.binning.OraNumericalAttributeBins;
import oracle.dmt.jdm.transform.binning.OraNumericalBin;
import oracle.dmt.jdm.transform.binning.OraNumericalBinningType;
import oracle.dmt.jdm.transform.clipping.OraClippingTransform;
import oracle.dmt.jdm.transform.clipping.OraClippingTransformFactory;
import oracle.dmt.jdm.transform.clipping.OraClippingType;
import oracle.dmt.jdm.transform.normalize.OraNormalizeTransform;
import oracle.dmt.jdm.transform.normalize.OraNormalizeTransformFactory;
import oracle.dmt.jdm.transform.normalize.OraNormalizeType;
import oracle.dmt.jdm.utils.OraSQLUtils;

public class dmxfdemo extends Object{
  //Connection related data members
  private static javax.datamining.resource.Connection m_dmeConn = null;
  private static javax.datamining.resource.ConnectionFactory m_dmeConnFactory = null;
  //Object factories used in this demo program
  private static OraTransformationFactory m_xformFactory = null;
  private static OraTransformationTaskFactory m_xformTaskFactory = null;
  private static OraBinningTransformFactory m_binXformFactory = null;
  // Global constant used for formatting 
  protected static DecimalFormat m_df = new DecimalFormat("00.##E0");

  public static void main( String args[] ) { 
    try {      
        if (( args.length != 0 ) & ( args.length != 3 )) {
          System.out.println("Usage: java dmxfdemo ");
          System.out.println("   or: java dmxfdemo <Host name>:<Port>:<SID> <User Name> <Password>");
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
        // 4. custom binning
        binDataCustom("MINING_DATA_BINNED_CUSTOM");
        // 5. discretize data using equal width
        binDataEqWidth();
        // 6. discretize data using quantile
        binDataQtile();
        // 7. bin using supervised method
        binSupervised();
        // 8. clip data using trimming
        clipDataTrim();
        // 9. clip data using winsorizing
        clipDataWinsorize();
        // 10. normalize data using min-max
        normalizeMinMax();
        // 11. normalize data using z-score
        normalizeZScore();
    } catch(Exception anyExp) {
      anyExp.printStackTrace(System.out);
    } finally {
      try {
        // 10. Logout from the Data Mining Engine
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
    m_xformFactory = (OraTransformationFactory)m_dmeConn.getFactory(
      "oracle.dmt.jdm.transform.OraTransformation");
    m_xformTaskFactory = (OraTransformationTaskFactory)m_dmeConn.getFactory(
      "oracle.dmt.jdm.task.OraTransformationTask");
    
    m_binXformFactory = (OraBinningTransformFactory)m_dmeConn.getFactory(
        "oracle.dmt.jdm.transform.binning.OraBinningTransform");

  }

  /**
   * Illustrates how to perform equal width data discretization
   * 
   * @exception JDMException if discretization failed
   */
  public static void binDataEqWidth() throws JDMException
  {
    binData("MINING_DATA_BINNED_EQW", OraNumericalBinningType.equi_width);
  }
  
  /**
   * Illustrates how to perform quantile data discretization
   * 
   * @exception JDMException if discretization failed
   */
  public static void binDataQtile() throws JDMException
  {
    binData("MINING_DATA_BINNED_QTL",OraNumericalBinningType.quantile);
  }
  
  /**
   * Illustrates how to perform data discretization.
   * 
   * @param resultXformName name of the result discretized view
   * @param binningType type of discretization to perform i.e., 
   * quantile, equal width or custom
   * @throws JDMException if discretization failed
   */
  public static void binData(String resultXformName, OraNumericalBinningType binningType) 
    throws JDMException
  {
    // Schema where the original data and resulting transformations reside
    String schema = ( m_dmeConn.getConnectionSpec().getName() ).toUpperCase();
    
    // Create discretization transformation instance
    OraBinningTransform obt = m_xformFactory.createBinningTransform();
    obt.setTransformInputData(schema + "." + "MINING_DATA_BUILD_V");
    obt.setTransformOutputData(schema + "." + resultXformName);
      
    // Specify the number of numeric bins
    obt.setNumberOfBinsForNumerical(10);

    // Specify the number of categoric bins
    obt.setNumberOfBinsForCategorical(8);

    // Specify the list of excluded attributes
    String[] excludedList = new String[]{
      "CUST_ID", "CUST_GENDER"
    };
    obt.setExcludeColumnList(excludedList);
    
    // Specify the type of numeric binning: equal-width or quantile 
    // ( default is quantile )
    obt.setNumericalBinningType(binningType);
    // Specify the type of categorical binning as Top-N: by default it is none   
    obt.setCategoricalBinningType(OraCategoricalBinningType.top_n);

    ArrayList xformList = new ArrayList();
    xformList.add(obt);
    //Create a transformation sequence object
    OraTransformationSequence xformSeq = 
      m_xformFactory.createTransformationSequence( 
          schema + "." + "MINING_DATA_BUILD_V", // name of the input data set
          xformList, //List of transformations. In this case only one type of transformation i.e., supervised binning
          schema + "." + resultXformName // name of the transformation result             
          );
    String xformSeqName =  "bin_" + binningType.name() + "_xfSeq";     
    m_dmeConn.saveObject(xformSeqName, xformSeq, true);
    
    OraTransformationTask xformTask = m_xformTaskFactory.create(
      xformSeqName, false);
          
    executeTask(xformTask, "xFormBin_jdm");    
    displayDiscretizationResults(
      binningType,
      schema + "." + resultXformName,
      new String[]{"CUST_INCOME_LEVEL","OCCUPATION"}
      );
  }
  
  /**
   * Illustrates how to perform custom data discretization. First discretization
   * of 2 numerical attributes is performed. "AGE" is binned with equal width
   * method with 10 bins and "YRS_RESIDENCE" with quantile method and 5 bins.
   * Categorical attributes "EDUCATION" and "OCCUPATION" are discretized
   * with the Top-N method into 15 and 10 bins. This method illustrates
   * how additional attributes can be added to the existing discretization
   * tables: "AFFINITY_CARD" and ""HOUSEHOLD_SIZE". Finally results are combined 
   * into a single array and custom transformation task is performed. 
   * 
   * @param resultXformName name of the result transformation view
   * @throws JDMException if transformation failed
   */
  public static void binDataCustom(String resultXformName) throws JDMException
  {
    System.out.println("Custom binning");
    System.out.println("--------------------------------------------");
    // Schema where the original data and resulting transformations reside
    String schema = ( m_dmeConn.getConnectionSpec().getName() ).toUpperCase();
    
    // Numeric custom binning
    OraNumericalAttributeBins[] customNumBins = m_binXformFactory.computeNumericBins(
      schema + "." + "MINING_DATA_BUILD_V", // name of the input data set
      new String[] {
        "AGE", "YRS_RESIDENCE", 
        },
      new OraNumericalBinningType[] {
        OraNumericalBinningType.equi_width, 
        OraNumericalBinningType.quantile
        },
      new Integer[] {new Integer(10), new Integer(5)}
    );
    
    if ( customNumBins == null ){
      System.out.println("Error: no numeric bins were computed");
      return;
    }
    
    //Categoric custom binning
    OraCategoricalAttributeBins[] customCatBins = m_binXformFactory.computeCategoricBins(
      schema + "." + "MINING_DATA_BUILD_V", // name of the input data set
      new String[] {
        "EDUCATION", "OCCUPATION", 
        },
      new OraCategoricalBinningType[] {
        OraCategoricalBinningType.top_n, 
        OraCategoricalBinningType.top_n, 
        },
      new Integer[] {new Integer(15), new Integer(10),}
    );
    
    if ( customCatBins == null ){
      System.out.println("Error: no categoric bins were computed");
      return;
    }

    // combine custom bins into the single array
    OraAttributeBins[] customBins = null;
    customBins = incrementArray(customBins, (OraAttributeBins[])customNumBins);
    customBins = incrementArray(customBins, (OraAttributeBins[])customCatBins);
    
    // show resulting array of custom bins
    for ( int i = 0 ; i <  customBins.length; i++ )  {
      System.out.println("Attribute:" + customBins[i].getAttributeName() );
      
      if ( customBins[i] instanceof OraNumericalAttributeBins ){
        OraNumericalAttributeBins oraNumBin = (OraNumericalAttributeBins)customBins[i];
        OraNumericalBin[] bs = oraNumBin.getBins();
        System.out.println("\tBin ID\tLower\tUpper" );
        for ( int j = 0 ; j <  bs.length; j++ )  {
          System.out.println("\t" + bs[j].getBinID() + "\t" + m_df.format(bs[j].getStartValue()) +
                                                       "\t" + m_df.format(bs[j].getEndValue()) );
        }
      }
      else if ( customBins[i] instanceof OraCategoricalAttributeBins ){
        OraCategoricalAttributeBins oraCatBin = (OraCategoricalAttributeBins)customBins[i];
        OraCategoricalBin[] bs = oraCatBin.getBins();
        System.out.println("\tBin ID\tCategory" );
        for ( int j = 0 ; j <  bs.length; j++ )  {
          Object[] categories = bs[j].getCategories();
          System.out.print("\t" + bs[j].getBinID() +"\t" );
          for ( int k = 0 ; k <  categories.length; k++ )  {
            System.out.print(categories[k].toString() );
            if ( k < categories.length - 1 )
               System.out.print(";");
          }
          System.out.println();
        }
      }
    }
    // Create discretization transformation instance
    OraBinningTransform obt = m_xformFactory.createBinningTransform(customBins);
      
    // Specify the type of numeric binning: custom
    obt.setNumericalBinningType(OraNumericalBinningType.custom);
    // Specify the type of categoric binning: custom
    obt.setCategoricalBinningType(OraCategoricalBinningType.custom);
    
    ArrayList xformList = new ArrayList();
    xformList.add(obt);
    //Create a transformation sequence object
    OraTransformationSequence xformSeq = 
    m_xformFactory.createTransformationSequence( 
        schema + "." + "MINING_DATA_BUILD_V", // name of the input data set
        xformList, //List of transformations. In this case only one type of transformation i.e., supervised binning
        schema + "." + resultXformName // name of the transformation result             
        );
    String xformSeqName =  "bin_" + OraNumericalBinningType.custom.name() + "_xfSeq";     
    m_dmeConn.saveObject(xformSeqName, xformSeq, true);
    
    OraTransformationTask xformTask = m_xformTaskFactory.create(xformSeqName, true);
    executeTask(xformTask, "xCustomBin_jdm");    
    
    // display content of the bin definition tables
    showBinDefinitionTableContents(obt.getCategoricalBinTable(), "categorical");
    showBinDefinitionTableContents(obt.getNumericalBinTable(), "numerical");

    displayDiscretizationResults(
      OraNumericalBinningType.custom,
      schema + "." + resultXformName,
      new String[]{"AGE", "CUST_INCOME_LEVEL","EDUCATION", "OCCUPATION"}
      );
      
    // Bin additional attribute and add 
    // to the existing bin definition tables
    //----------------------------------------------------------------
    // Numeric custom binning
    OraNumericalAttributeBins[] customNumBinsAdd = m_binXformFactory.computeNumericBins(
      schema + "." + "MINING_DATA_BUILD_V", // name of the input data set
      new String[] {
        "AFFINITY_CARD", 
        },
      new OraNumericalBinningType[] {
        OraNumericalBinningType.quantile, 
        },
      new Integer[] {new Integer(6)}
    );
    
    if ( customNumBins == null ){
      System.out.println("Error: no numeric bins were computed");
      return;
    }
    
    //Categoric custom binning
    OraCategoricalAttributeBins[] customCatBinsAdd = m_binXformFactory.computeCategoricBins(
      schema + "." + "MINING_DATA_BUILD_V", // name of the input data set
      new String[] {
        "HOUSEHOLD_SIZE", 
        },
      new OraCategoricalBinningType[] {
        OraCategoricalBinningType.top_n,  
        },
      new Integer[] {new Integer(15), new Integer(10),}
    );
    
    if ( customCatBins == null ){
      System.out.println("Error: no categoric bins were computed");
      return;
    }

    // combine custom bins into the single array
    OraAttributeBins[] customBinsAdd = null;
    customBinsAdd = incrementArray(customBinsAdd, (OraAttributeBins[])customNumBinsAdd);
    customBinsAdd = incrementArray(customBinsAdd, (OraAttributeBins[])customCatBinsAdd);
    
    // clean up previous view
    dropView ( null, resultXformName);
    
    OraBinningTransform obtAdd = m_xformFactory.createBinningTransform(
      obt.getCategoricalBinTable(),
      obt.getNumericalBinTable(), 
      customBinsAdd);
    obtAdd.setTransformInputData(schema + "." + "MINING_DATA_BUILD_V");
    obtAdd.setTransformOutputData(schema + "." + resultXformName);
      
    // Specify the type of numeric binning: custom
    obtAdd.setNumericalBinningType(OraNumericalBinningType.custom);
    // Specify the type of categoric binning: custom
    obtAdd.setCategoricalBinningType(OraCategoricalBinningType.custom);
    
    OraTransformationTask xformTaskAdd = m_xformTaskFactory.create(obtAdd);
    executeTask(xformTaskAdd, "xCustomBinAdd_jdm");    

    // display content of the new bin definition tables
    showBinDefinitionTableContents(obtAdd.getCategoricalBinTable(), "categorical");
    showBinDefinitionTableContents(obtAdd.getNumericalBinTable(), "numerical");
    
    // show results
    displayDiscretizationResults(
      OraNumericalBinningType.custom,
      schema + "." + resultXformName,
      new String[]{"AFFINITY_CARD","HOUSEHOLD_SIZE"}
      );
  }
  
  /**
   * For supervised functions with the known target attribute, supervised 
   * binning is a recommended approach. It is a smart binning based on the
   * target attribute values. This method is supported from 11.1 release
   * of ODM. 
   */
  public static void binSupervised() throws JDMException {
    // Schema where the original data and resulting transformations reside
    String schema = ( m_dmeConn.getConnectionSpec().getName() ).toUpperCase();
    
    // Create discretization transformation instance
    OraBinningTransform obt = m_xformFactory.createBinningTransform();
    
    
    // Specify the list of excluded attributes
    String[] excludedList = new String[]{
    "CUST_ID", "CUST_GENDER"
    };
    obt.setExcludeColumnList(excludedList);
    
    // Specify the type of numeric binning: supervised
    obt.setNumericalBinningType(OraNumericalBinningType.supervised);
    // Specify the type of categorical binning as supervised   
    obt.setCategoricalBinningType(OraCategoricalBinningType.supervised);
    obt.setTargetAttributeName("AFFINITY_CARD");
    
    ArrayList xformList = new ArrayList();
    xformList.add(obt);
    //Create a transformation sequence object
    OraTransformationSequence xformSeq = 
        m_xformFactory.createTransformationSequence( 
            schema + "." + "MINING_DATA_BUILD_V", // name of the input data set
            xformList, //List of transformations. In this case only one type of transformation i.e., supervised binning
            schema + "." + "MINING_DATA_BINNED_SUP" // name of the transformation result             
            );
    m_dmeConn.saveObject("superBin_xformSeq", xformSeq, true);
    
    OraTransformationTask xformTask = m_xformTaskFactory.create(
        "superBin_xformSeq", false);
    executeTask(xformTask, "xFormSuperBin_jdm");    
    displayDiscretizationResults(
        OraNumericalBinningType.supervised,
        schema + "." + "MINING_DATA_BINNED_SUP",
        new String[]{"CUST_INCOME_LEVEL","OCCUPATION"}
    );
  }
  
  /**
   * Shows histogram for selected binned attributes
   * 
   * @param binningType type of discretization performed i.e., quantile, equal 
   * width or custom
   * @param xformResult name of the result discretized view
   * @param attributes names of attributes for which histogram is displayed
   */
  public static void displayDiscretizationResults(
    OraNumericalBinningType binningType, String xformResult, String[] attributes) 
  {
    System.out.println("\nShowing results of the discretization transformation");
    System.out.println("\tType of discretization: " + binningType.name() );
    java.sql.Connection dbConn = 
      ((OraConnection)m_dmeConn).getDatabaseConnection();
    for ( int i = 0; i < attributes.length; i++ )  {
      String sqlQuery = MessageFormat.format(
        "SELECT {0} BIN_NUMBER, COUNT(*) FREQUENCY FROM ({1}) GROUP BY {0} " + 
        "ORDER BY FREQUENCY DESC,BIN_NUMBER ASC",
        new String[]{
          "\""+attributes[i]+"\"", 
          xformResult
          } 
      );
      Statement stmt = null;
      ResultSet rs = null;
      System.out.println("\tHistogram for:" + attributes[i]);
      try{      
        stmt = dbConn.createStatement();
        rs = stmt.executeQuery(sqlQuery);
        while ( rs.next() ){
          String binValue = rs.getString("BIN_NUMBER");
          int freq = rs.getInt("FREQUENCY");
          System.out.println("\t\t"+binValue + " " + freq);
        }
      }
      catch(Exception e){
        System.out.println(e);
      }
      finally{
        try{rs.close(); stmt.close();}
        catch(Exception e){}
      }
    }
  }


  /**
   * Shows results of the clipping for selected attributes
   * 
   * @param schema schema where result transformation view resides
   * @param xformResult name of the resulting transformation view 
   * @param attribute name of the attribute for which transformation was performed
   * @param clippingType type of clipping which was performed, i.e. 
   *        trimming or winsorising
   */
  public static void displayClippingResults( String schema, String xformResult, 
    String attribute, OraClippingType clippingType) 
  {
    System.out.println("\nShowing results of the clipping transformation");
    System.out.println("\tClipping type: " + 
      ( true == clippingType.equals(OraClippingType.trim) ? "trim" : "winsorize") );
    System.out.println("\tMinimum and maximum values for:" + attribute + 
      " before clipping transformation");

    String sqlQuery = MessageFormat.format(
      "SELECT MIN ({0}) MIN_VALUE, MAX ({0}) MAX_VALUE FROM ({1})",
      new String[]{ "\""+attribute+"\"",  
        schema + "." + "MINING_DATA_BUILD_V"}
    );
    getMinMax(sqlQuery);
    
    System.out.println("\tMinimum and maximum values for:" + attribute + 
      " after clipping transformation.");
    sqlQuery = MessageFormat.format(
      "SELECT MIN ({0}) MIN_VALUE, MAX ({0}) MAX_VALUE FROM ({1})",
      new String[]{ "\""+attribute+"\"",  
        schema + "." + xformResult}
    );
    getMinMax(sqlQuery);
  }

  private static void getMinMax(String sqlQuery)
  {
    java.sql.Connection dbConn = 
      ((OraConnection)m_dmeConn).getDatabaseConnection();
    Statement stmt = null;
    ResultSet rs = null;
    try{      
      stmt = dbConn.createStatement();
      rs = stmt.executeQuery(sqlQuery);
      while ( rs.next() ){
        String minValue = rs.getString("MIN_VALUE");
        String maxValue = rs.getString("MAX_VALUE");
        System.out.println(
          "\t\tMin value=" + m_df.format(Double.parseDouble(minValue) ) + 
          ", Max value=" + m_df.format(Double.parseDouble(maxValue)));
      }
    }
    catch(Exception e){
      System.out.println(e);
    }
    finally{
      try{rs.close(); stmt.close();}
      catch(Exception e){}
    }
  }

  private static void clean()
  {
    java.sql.Connection dbConn = 
      ((OraConnection)m_dmeConn).getDatabaseConnection();

    dropView( dbConn, "MINING_DATA_BINNED_EQW");
    dropView( dbConn, "MINING_DATA_BINNED_QTL");
    dropView( dbConn, "MINING_DATA_CLIP_TRIM");
    dropView( dbConn, "MINING_DATA_CLIP_WINS");
    dropView( dbConn, "MINING_DATA_BINNED_CUSTOM");
    dropView( dbConn, "MINING_DATA_NORMALIZE_MIN_MAX");
    dropView( dbConn, "MINING_DATA_NORMALIZE_Z_SCORE");
  }
  
  private static void dropView(java.sql.Connection dbConn, String viewName)
  {
    if ( dbConn == null )
      dbConn = ((OraConnection)m_dmeConn).getDatabaseConnection();
      
    Statement stmt = null;
    try 
    {
      stmt = dbConn.createStatement();
      stmt.executeUpdate("DROP VIEW " + viewName);  
    } catch(SQLException anySqlExp) {}//Ignore
    finally{
      try{stmt.close();}
      catch(SQLException anySqlExp) {}
    }
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
    System.out.print("\n"+taskName + " is started, please wait. ");
    //Wait for completion of the task
    ExecutionStatus status = execHandle.waitForCompletion(Integer.MAX_VALUE);    
    //Check the status of the task after completion
    isTaskSuccess = status.getState().equals(ExecutionState.success);
    if( isTaskSuccess ) {
      //Task completed successfully
      System.out.println(taskName + " is successful.\n");
    } else {//Task failed
      System.out.println(taskName + " is failed.\nFailure Description: " + 
        status.getDescription() + "\n" );
    }
    return isTaskSuccess;
  }

  /**
   * Illustrates how to perform trimming type of clipping which involves
   * removing the tail values of the attribute.
   * 
   * @throws JDMException if transformation failed
   */
  public static void clipDataTrim() throws JDMException
  {
    clipData(OraClippingType.trim, "MINING_DATA_CLIP_TRIM");
  }

  /**
   * Illustrates how to perform winsorising type of clipping which involves
   * setting the tail values of a particular attribute to some specified 
   * quantile of the data.
   * 
   * @throws JDMException if transformation failed
   */
  public static void clipDataWinsorize() throws JDMException
  {
    clipData(OraClippingType.winsorize, "MINING_DATA_CLIP_WINS");
  }  
  /**
   * Illustrates how to perform data clipping
   * 
   * @param clippingType type of clipping to perform
   * @param xformResult name of the result transformation view
   * @throws JDMException if transformation failed
   */
  public static void clipData(OraClippingType clippingType, String xformResult) 
    throws JDMException
  {
    // Schema where the original data and resulting transformations reside
    String schema = ( m_dmeConn.getConnectionSpec().getName() ).toUpperCase();

    OraClippingTransform oct = m_xformFactory.createClippingTransform();
    oct.setTransformInputData(schema + "." + "MINING_DATA_BUILD_V");
    oct.setTransformOutputData(schema + "." + xformResult);
    
    // Specify the list of excluded attributes
    String[] excludedList = new String[]{
      "CUST_ID", "CUST_GENDER"
    };
    oct.setExcludeColumnList(excludedList);
    
    // Specify the type of clipping: trim of winsorize ( default is trimming).
    oct.setClippingType(clippingType);
    
    // Specify the tail fraction as 3% of values on both ends
    oct.setTailFraction(0.03);
    
    ArrayList xformList = new ArrayList();
    xformList.add(oct);
    //Create a transformation sequence object
    OraTransformationSequence xformSeq = 
    m_xformFactory.createTransformationSequence( 
        schema + "." + "MINING_DATA_BUILD_V", // name of the input data set
        xformList, //List of transformations. In this case only one type of transformation i.e., supervised binning
        schema + "." + xformResult // name of the transformation result             
        );
    String xformSeqName =  "clp_" + clippingType.name() + "_xfSeq";     
    m_dmeConn.saveObject(xformSeqName, xformSeq, true);
    
    OraTransformationTask xformTask = m_xformTaskFactory.create(
        xformSeqName, false);
    executeTask(xformTask, "xfromClip_jdm");    

    displayClippingResults( schema, xformResult, "AGE", clippingType);
  }

  /**
   * Illustrates how to perform Min-Max Normalization. The normalization 
   * definition for each attribute is computed based on the min and max 
   * values that are computed from the data. The values for shift and scale 
   * are computed to be shift = min, and scale = (max - min) respectively.
   * 
   * @throws JDMException if transformation failed
   */
  public static void normalizeMinMax() throws JDMException
  {
    normalizeData(OraNormalizeType.min_max, "MINING_DATA_NORMALIZE_MIN_MAX");
  }
  
  /**
   * Illustrates how to perform Z-Score Normalization. The normalization 
   * definition for each attribute is computed based on the values for mean 
   * and standard deviation that are computed from the data. The values for 
   * shift and scale are computed to be shift = mean, and scale = standard 
   * deviation respectively.
   * 
   * @throws JDMException if transformation failed
   */
  public static void normalizeZScore() throws JDMException
  {
    normalizeData(OraNormalizeType.z_Score, "MINING_DATA_NORMALIZE_Z_SCORE"); 
  }
  
  /**
   * Illustrates how to perform data normalization
   * 
   * @param normalizeType type of normalization to perform
   * @param xformResult name of the result transformation view
   * @throws JDMException if transformation failed
   */
  public static void normalizeData(OraNormalizeType normalizeType, String xformResult) 
    throws JDMException
  {
    // Schema where the original data and resulting transformations reside
    String schema = ( m_dmeConn.getConnectionSpec().getName() ).toUpperCase();

    OraNormalizeTransform ont = m_xformFactory.createNormalizeTransform(      
      normalizeType,
      new Integer(6)
      );
    
    // Specify the list of excluded attributes
    String[] excludedList = new String[]{
      "CUST_ID", "CUST_GENDER"
    };
    ont.setExcludeColumnList(excludedList);
    
    ArrayList xformList = new ArrayList();
    xformList.add(ont);
    //Create a transformation sequence object
    OraTransformationSequence xformSeq = 
    m_xformFactory.createTransformationSequence( 
        schema + "." + "MINING_DATA_BUILD_V", // name of the input data set
        xformList, //List of transformations. In this case only one type of transformation i.e., supervised binning
        schema + "." + xformResult // name of the transformation result             
        );
    String xformSeqName =  "nmz_" + normalizeType.name() + "_xfSeq";     
    m_dmeConn.saveObject(xformSeqName, xformSeq, true);
    
    OraTransformationTask xformTask = m_xformTaskFactory.create(
      xformSeqName, false);
    executeTask(xformTask, "xformNormalize_jdm");    
    displayNormalizeResults( schema, "AGE", normalizeType, xformResult);
  }

  /**
   * Shows results of the normalization for selected attribute
   * 
   * @param schema schema where result transformation view resides
   * @param attribute name of the attribute which was normalized
   * @param normalizeType type of normalization performed
   * @param xformResult name of the result transformation view
   */
  public static void displayNormalizeResults(String schema, 
                                             String attribute, 
                                             OraNormalizeType normalizeType,
                                             String xformResult) 
  {
    System.out.println("\nShowing results of the normalization transformation");
    System.out.println("\tNormalize type: " + 
      ( true == normalizeType.equals(OraNormalizeType.min_max) ? "min_max" : "z_score") );
    System.out.println("\tMinimum and maximum values for:" + attribute + 
      " before normalize transformation");

    String sqlQuery = MessageFormat.format(
      "SELECT MIN ({0}) MIN_VALUE, MAX ({0}) MAX_VALUE FROM ({1})",
      new String[]{ "\""+attribute+"\"",  
        schema + "." + "MINING_DATA_BUILD_V"}
    );
    getMinMax(sqlQuery);
    
    System.out.println("\tMinimum and maximum values for:" + attribute + 
      " after normalize transformation");
    sqlQuery = MessageFormat.format(
      "SELECT MIN ({0}) MIN_VALUE, MAX ({0}) MAX_VALUE FROM ({1})",
      new String[]{ "\""+attribute+"\"",  
        schema + "." + xformResult}
    );
    getMinMax(sqlQuery);
  }
  
  private static void showBinDefinitionTableContents(String tableName, String tableType)
  {
    //Bin definition tables have 3 columns
    java.sql.Connection dbConn = ((OraConnection)m_dmeConn).getDatabaseConnection();
    String sql = "SELECT * FROM " + tableName;
    
    try{
      Vector results = OraSQLUtils.execSqlQuery(dbConn, sql, null );
      System.out.println("The contents of the " + tableType + " bin definition table.");
      System.out.println("\n\tCOL\t\tVAL\t\tBIN");
      for ( int row = 0 ;  row < results.size(); row++ )  {
        StringBuffer sb = new StringBuffer("\t");
        Object element = results.elementAt(row);
        if ( element == null )
          throw new Exception("Failure reading " + tableType +
            " table. Reason: data is null");
        if ( false == element instanceof Object[] )
          throw new Exception("Failure reading " + tableType +
            " table. Wrong data format.");
        Object[] singleRow = (Object[])element;
        for ( int col = 0 ;  col < singleRow.length; col++ )  {
          if ( singleRow[col] != null && singleRow[col] instanceof BigDecimal )
            sb.append( m_df.format(singleRow[col]) + "\t\t");
          else  
            sb.append( (singleRow[col] != null ? singleRow[col].toString() : "") 
            + "\t\t");
        }
        System.out.println(sb.toString());
      }
    }
    catch(Exception e){
      System.out.println("Failed to display the contents of the table: " + 
        tableName + ". Reason: ");
    }
  }
  
  private static OraAttributeBins[] incrementArray(OraAttributeBins[] srcArray, 
        OraAttributeBins[] newArray)
  {
    if ( newArray == null )
      return srcArray;
    
    // if final array has not been created yet, check 
    // wheather new array contains data
    if ( srcArray == null ) {
      if (newArray == null)
        return null;
      
      // if the new array has data, allocate a new array,
      // copies the newArray into the resultArray and returns resultArray
      OraAttributeBins[] resultArray = new OraAttributeBins[newArray.length];
      System.arraycopy(newArray, 0, resultArray, 0, newArray.length);
      return resultArray;
    }
      
    //srcArray already has some data
    OraAttributeBins[] resultArray = new OraAttributeBins[srcArray.length + 
      newArray.length];
    // copy original data into the result array
    System.arraycopy(srcArray, 0, resultArray, 0, srcArray.length);
    // copy the new data into the result array
    System.arraycopy(newArray, 0, resultArray, srcArray.length, newArray.length);
    return resultArray;
  }
}
