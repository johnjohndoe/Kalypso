package de.renew.workflow;

public interface EclipseContextTask extends ConfirmedTask
{

  public static final String PARAM_CMD_ID = "commandId";

  public static final String RUNTIME_PARAMETER = "runtimeParameter";

  public interface WorkItem extends ConfirmedTask.WorkItem
  {

  }
}
