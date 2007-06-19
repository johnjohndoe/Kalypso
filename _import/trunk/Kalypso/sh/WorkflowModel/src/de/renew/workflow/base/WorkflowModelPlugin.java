package de.renew.workflow.base;

import org.eclipse.core.runtime.Plugin;

public class WorkflowModelPlugin extends Plugin
{
  public static final String ID = "de.renew.workflow.model";
  
  private static WorkflowModelPlugin m_instance;

  public WorkflowModelPlugin( )
  {
    m_instance = this;
  }

  public static WorkflowModelPlugin getInstance( )
  {
    return m_instance;
  }

}
