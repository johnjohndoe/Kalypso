package org.kalypso.eclipse.core.runtime.jobs;

import org.eclipse.core.runtime.jobs.ISchedulingRule;

/**
 * MutexSchedulingRule
 * 
 * @author schlienger
 */
public class MutexSchedulingRule implements ISchedulingRule
{
  /**
   * @see org.eclipse.core.runtime.jobs.ISchedulingRule#contains(org.eclipse.core.runtime.jobs.ISchedulingRule)
   */
  public boolean contains( ISchedulingRule rule )
  {
    return true;
    // TODO: ge�ndert von Belger: sonst gibts Probleme beim Laden der Zeitreihen
    // --> Pr�fen ob das immer noch der Fall ist?
//    return this == rule;
  }

  /**
   * @see org.eclipse.core.runtime.jobs.ISchedulingRule#isConflicting(org.eclipse.core.runtime.jobs.ISchedulingRule)
   */
  public boolean isConflicting( ISchedulingRule rule )
  {
    return this == rule;
  }
}
