package org.kalypso.transformation;

import org.kalypsodeegree.KalypsoDeegreePlugin;

/**
 * This class is a helper for debugging.
 * 
 * @author Holger Albert
 */
public class Debug
{
  /**
   * The constructor.
   */
  private Debug( )
  {
  }

  public static final org.kalypso.contribs.eclipse.core.runtime.Debug TRANSFORM = new org.kalypso.contribs.eclipse.core.runtime.Debug( KalypsoDeegreePlugin.getDefault(), "/debug/transformation" );
}