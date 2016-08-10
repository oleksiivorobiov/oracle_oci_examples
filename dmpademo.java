// Copyright (c) 2004, 2006, Oracle. All rights reserved.  
// File: dmpademo.java
// Generic api imports
import java.sql.Statement;
// Java Data Mining (JDM) standard imports
import javax.datamining.ExecutionHandle;
import javax.datamining.ExecutionState;
import javax.datamining.ExecutionStatus;
import javax.datamining.JDMException;
import javax.datamining.NamedObject;
import javax.datamining.base.Task;
import javax.datamining.resource.Connection;
import javax.datamining.resource.ConnectionFactory;
import javax.datamining.resource.ConnectionSpec;
// Oracle Java Data Mining (JDM) implemented api imports
import oracle.dmt.jdm.base.OraTask;
import oracle.dmt.jdm.resource.OraConnection;
import oracle.dmt.jdm.resource.OraConnectionFactory;
import oracle.dmt.jdm.task.OraExplainTask;
import oracle.dmt.jdm.task.OraExplainTaskFactory;
import oracle.dmt.jdm.task.OraPredictTask;
import oracle.dmt.jdm.task.OraPredictTaskFactory;
import oracle.dmt.jdm.task.OraPredictiveAnalyticsTaskFactory;
import oracle.dmt.jdm.task.OraProfileTask;

/**
* Predictive Analytics Demo. This demo runs the predict, explain and profile
* tasks. 
*
* ------------------------------------------------------------------------------
*                             EXECUTING DEMO PROGRAM
* ------------------------------------------------------------------------------
*   Refer to Oracle Data Mining Administrator's Guide
*   for guidelines for executing this demo program.
*/
public class

dmpademo
  extends Object
{

  //Connection related data members
  private static Connection m_dmeConn = null;
  private static ConnectionFactory m_dmeConnFactory = null;
  //Object factories used in this demo program
  private static OraPredictiveAnalyticsTaskFactory m_paFactory = null;

  public static void main(String[] args)
  {
    try
    {
      if (args.length != 3)
      {
        System.out.println("Usage: java dmpademo <Host name>:<Port>:<SID> <User Name> <Password>");
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
      // 2. Initialize factories for mining objects
      initFactories();
      // 3. Predict
      predict();
      // 4. Explain
      explain();
      // 5. Profile
      profile();
    }
    catch (Exception anyExp)
    {
      anyExp.printStackTrace(System.out);
    }
    finally
    {
      try
      {
        // 6. Logout from the Data Mining Engine
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
    m_paFactory =
        (OraPredictiveAnalyticsTaskFactory) m_dmeConn.getFactory("oracle.dmt.jdm.task.OraPredictiveAnalyticsTask");
  }

  /**
   * This method builds a mining model. The build operation requires the training
   * dataset location details and the mining function settings.
   * In this example, we will use the original unprepared dataset
   * "MINING_DATA_BUILD_V" as an input. The predictTask will automatically
   * prepare the dataset and create a new prepared dataset called
   * "JDM_PRED_MINING_DATA_BUILD".  Then, we will build a classification model
   * using the prepared build dataset in the user schema, with the NaiveBayes
   * algorithm.  This model can be used to predict which customer would respond
   * to a promotion campaign.
   */
  public static void predict()
    throws JDMException
  {
    //1. Create, save & execute predict Task
    OraPredictTask predictTask =
      m_paFactory.createPredictTask("MINING_DATA_BUILD_V", "cust_id",
                                    "affinity_card",
                                    "JDM_PRED_MINING_DATA_BUILD");
    predictTask.setDescription("PredictTask_jdm");
    ((OraTask)predictTask).overwriteOutput(true);
    executeTask(predictTask, "PredictTask_jdm");
    //Retrieve task & see the details of the task (Note:Remove this from demo program)
    try
    {
      predictTask =
          (OraPredictTask) m_dmeConn.retrieveObject("PredictTask_jdm",
                                                    NamedObject.task);
      predictTask.getInputData();
      predictTask.getOutputData();
      predictTask.getTargetAttributeName();
    }
    catch (Exception anyExp)
    {
      anyExp.printStackTrace();
    }
  }

  public static void explain()
    throws JDMException
  {
    //1. Create, save & execute predict Task
    OraExplainTask explainTask =
      m_paFactory.createExplainTask("MINING_DATA_BUILD_V", "affinity_card",
                                    "JDM_EXPL_MINING_DATA_BUILD");
    explainTask.setDescription("ExplainTask_jdm");
    ((OraTask)explainTask).overwriteOutput(true);
    executeTask(explainTask, "ExplainTask_jdm");
    try
    {
      explainTask =
          (OraExplainTask) m_dmeConn.retrieveObject("ExplainTask_jdm",
                                                    NamedObject.task);
      explainTask.getInputData();
      explainTask.getOutputData();
      explainTask.getExplainAttributeName();
    }
    catch (Exception anyExp)
    {
      anyExp.printStackTrace();
    }
  }

  /**
     * This method builds a mining model. The build operation requires the training
     * dataset location details and the mining function settings.
     * In this example, we will use the original unprepared dataset
     * "MINING_DATA_BUILD_V" as an input. The predictTask will automatically
     * prepare the dataset and create a new prepared dataset called
     * "JDM_PRED_MINING_DATA_BUILD".  Then, we will build a classification model
     * using the prepared build dataset in the user schema, with the NaiveBayes
     * algorithm.  This model can be used to predict which customer would respond
     * to a promotion campaign.
     */
  public static void profile()
    throws JDMException
  {
    //1. Create, save & execute predict Task
    OraProfileTask profileTask =
      m_paFactory.createProfileTask("MINING_DATA_BUILD_V", "affinity_card",
                                    "JDM_PROFILE_RESULTS");
    profileTask.setDescription("ProfileTask_jdm");
    ((OraTask)profileTask).overwriteOutput(true);
    executeTask(profileTask, "ProfileTask_jdm");
    //Retrieve task & see the details of the task (Note:Remove this from demo program)
    try
    {
      profileTask =
          (OraProfileTask) m_dmeConn.retrieveObject("ProfileTask_jdm",
                                                    NamedObject.task);
      profileTask.getInputData();
      profileTask.getTargetAttributeName();
    }
    catch (Exception anyExp)
    {
      anyExp.printStackTrace();
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
}
