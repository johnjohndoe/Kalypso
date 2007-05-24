package de.renew.workflow.base;

import org.eclipse.core.runtime.Plugin;

public class WorkflowModelPlugin extends Plugin
{

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
