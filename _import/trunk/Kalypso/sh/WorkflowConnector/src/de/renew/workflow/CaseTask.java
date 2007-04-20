package de.renew.workflow;


public interface CaseTask extends Task
{

  public static final String PARAM_CMD_ID = "commandId";

  public static final String RUNTIME_PARAMETER = "runtimeParameter";

  public interface WorkItem extends ConfirmedTask.WorkItem
  {

  }
}
