/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 * 
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestraﬂe 22
 *  21073 Hamburg, Germany
 *  http://www.tuhh.de/wb
 * 
 *  and
 *  
 *  Bjoernsen Consulting Engineers (BCE)
 *  Maria Trost 3
 *  56070 Koblenz, Germany
 *  http://www.bjoernsen.de
 * 
 *  This library is free software; you can redistribute it and/or
 *  modify it under the terms of the GNU Lesser General Public
 *  License as published by the Free Software Foundation; either
 *  version 2.1 of the License, or (at your option) any later version.
 * 
 *  This library is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 *  Lesser General Public License for more details.
 * 
 *  You should have received a copy of the GNU Lesser General Public
 *  License along with this library; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 * 
 *  Contact:
 * 
 *  E-Mail:
 *  belger@bjoernsen.de
 *  schlienger@bjoernsen.de
 *  v.doemming@tuhh.de
 *   
 *  ---------------------------------------------------------------------------*/
package org.kalypso.model.product.application;

import org.eclipse.ui.application.WorkbenchAdvisor;
import org.eclipse.ui.internal.ide.application.DelayedEventsProcessor;
import org.kalypso.afgui.perspective.Perspective;
import org.kalypso.contribs.eclipse.ide.application.IDEApplicationCopy;

/**
 * @author Holger Albert
 */
@SuppressWarnings("restriction")
public class KalypsoModelApplication extends IDEApplicationCopy
{
  /**
   * System property which determines the mode in which this application will run.
   * <p>
   * Ifset to false (or not defined), the user interface will be restricted to only those perspectives which are
   * automatically opened by the welcome page actions. Especially, the toolbars are hidden and the menu is restricted to
   * very view items like 'about'.
   * </p>
   * <p>
   * If set to true, the normal IDE-menues and toolbar is shown.
   * </p>
   */
  private static final String SYSPROP_EXPERT_MODE = "kalypso.model.product.expert"; //$NON-NLS-1$

  /**
   * If set, this perspective is forced to be open on startup. Default to the Workflow perspective.
   */
  private static final String SYSPROP_INITIAL_PERSPECTIVE = "kalypso.model.product.perspective"; //$NON-NLS-1$

  /**
   * How we handle the intro. Allowed values are 'always' (default), 'never', 'eclipse' (eclipse default behavior). If
   * set, the intro (Welcome View) is never shown
   */
  private static final String SYSPROP_INTOR_BEHAVIOUR = "kalypso.model.product.intro"; //$NON-NLS-1$

  /**
   * @see org.kalypso.model.product.application.IDEApplicationCopy#createWorkbenchAdvisor(org.eclipse.ui.internal.ide.application.DelayedEventsProcessor)
   */
  @Override
  protected WorkbenchAdvisor createWorkbenchAdvisor( final DelayedEventsProcessor processor )
  {
    final String initialPerspective = System.getProperty( SYSPROP_INITIAL_PERSPECTIVE, Perspective.ID );

    final boolean isExpert = Boolean.getBoolean( SYSPROP_EXPERT_MODE );

    return new KalypsoModelWorkbenchAdvisor( processor, !isExpert, initialPerspective );
  }

  public static IntroBehavior getIntroBehavior( )
  {
    final String introBehaviorName = System.getProperty( SYSPROP_INTOR_BEHAVIOUR, IntroBehavior.always.name() );
    return IntroBehavior.valueOf( introBehaviorName );
  }
}