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
package org.kalypso.ui.rrm.internal.scenarios;

import org.eclipse.core.runtime.IStatus;
import org.eclipse.jface.viewers.ColumnLabelProvider;
import org.eclipse.swt.graphics.Font;
import org.eclipse.swt.graphics.Image;
import org.kalypso.core.status.StatusComposite;

import de.renew.workflow.connector.cases.IScenario;

/**
 * This label provider provides labels for scenarios
 * 
 * @author Holger Albert
 */
public class ScenarioCompareStatusLabelProvider extends ColumnLabelProvider
{
  /**
   * The scenario compare status contains stati for several cases.
   */
  private final ScenarioCompareStatus m_compareStatus;

  /**
   * The key for which the status will be retrieved.
   */
  private final String m_key;

  /**
   * The target scenario.
   */
  private final IScenario m_targetSenario;

  /**
   * The constructor.
   * 
   * @param compareStatus
   *          The scenario compare status contains stati for several cases.
   * @param key
   *          The key for which the status will be retrieved.
   * @param targetSenario
   *          The target scenario.
   */
  public ScenarioCompareStatusLabelProvider( final ScenarioCompareStatus compareStatus, final String key, final IScenario targetSenario )
  {
    m_compareStatus = compareStatus;
    m_key = key;
    m_targetSenario = targetSenario;
  }

  /**
   * @see org.eclipse.jface.viewers.ColumnLabelProvider#getImage(java.lang.Object)
   */
  @Override
  public Image getImage( final Object element )
  {
    if( element.equals( m_targetSenario ) )
      return null;

    if( element instanceof IScenario )
    {
      final IScenario scenario = (IScenario) element;
      final String uri = scenario.getURI();
      final IStatus status = m_compareStatus.getStatus( uri, m_key );
      if( status != null )
        return StatusComposite.getStatusImage( status );

      return super.getImage( element );
    }

    return super.getImage( element );
  }

  /**
   * @see org.eclipse.jface.viewers.ColumnLabelProvider#getText(java.lang.Object)
   */
  @Override
  public String getText( final Object element )
  {
    if( element.equals( m_targetSenario ) )
      return ""; //$NON-NLS-1$

    if( element instanceof IScenario )
    {
      final IScenario scenario = (IScenario) element;
      final String uri = scenario.getURI();
      final IStatus status = m_compareStatus.getStatus( uri, m_key );
      if( status != null )
        return status.getMessage();

      return ""; //$NON-NLS-1$
    }

    return ""; //$NON-NLS-1$
  }

  /**
   * @see org.eclipse.jface.viewers.ColumnLabelProvider#getFont(java.lang.Object)
   */
  @Override
  public Font getFont( final Object element )
  {
    return null;
  }
}