package org.kalypso.ui.application;

import org.eclipse.ui.internal.ide.IDEWorkbenchAdvisor;

/**
 * @author belger
 */
public class KalypsoWorkbenchAdvisor extends IDEWorkbenchAdvisor
{
  public KalypsoWorkbenchAdvisor()
  {
    super();
  }

  /**
   * @see org.eclipse.ui.application.WorkbenchAdvisor#getInitialWindowPerspectiveId()
   */
  public String getInitialWindowPerspectiveId()
  {
    return "org.kalypso.ui.perspectives.ObservationRepositoryPerspectiveFactory";
  }
}
