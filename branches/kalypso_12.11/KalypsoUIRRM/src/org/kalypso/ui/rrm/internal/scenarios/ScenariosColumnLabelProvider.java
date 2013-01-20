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

import org.eclipse.jface.resource.JFaceResources;
import org.eclipse.jface.viewers.ColumnLabelProvider;
import org.eclipse.swt.graphics.Font;
import org.eclipse.swt.graphics.Image;
import org.kalypso.afgui.workflow.WorkflowBreadcrumbLabelProvider;
import org.kalypso.ui.rrm.internal.i18n.Messages;

import de.renew.workflow.connector.cases.IScenario;

/**
 * This label provider provides labels for scenarios
 * 
 * @author Holger Albert
 */
public class ScenariosColumnLabelProvider extends ColumnLabelProvider
{
  /**
   * The target scenario.
   */
  private final IScenario m_targetScenario;

  /**
   * The delegate label provider.
   */
  private final WorkflowBreadcrumbLabelProvider m_delegateLabelProvider;

  /**
   * The constructor.
   * 
   * @param targetScenario
   *          The target scenario-
   */
  public ScenariosColumnLabelProvider( final IScenario targetScenario )
  {
    m_targetScenario = targetScenario;
    m_delegateLabelProvider = new WorkflowBreadcrumbLabelProvider();
  }

  @Override
  public Image getImage( final Object element )
  {
    return m_delegateLabelProvider.getImage( element );
  }

  @Override
  public String getText( final Object element )
  {
    final String text = m_delegateLabelProvider.getText( element );
    if( element.equals( m_targetScenario ) )
      return String.format( Messages.getString( "ScenariosColumnLabelProvider_0" ), text ); //$NON-NLS-1$

    return text;
  }

  @Override
  public Font getFont( final Object element )
  {
    if( element.equals( m_targetScenario ) )
      return JFaceResources.getFontRegistry().getBold( JFaceResources.DEFAULT_FONT );

    return null;
  }
}