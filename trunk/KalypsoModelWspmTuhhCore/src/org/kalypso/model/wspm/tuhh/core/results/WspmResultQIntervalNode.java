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
package org.kalypso.model.wspm.tuhh.core.results;

import org.eclipse.core.runtime.IPath;
import org.kalypso.model.wspm.tuhh.core.gml.TuhhCalculation;
import org.kalypso.model.wspm.tuhh.core.i18n.Messages;

/**
 * This result container represents a WSPM result. TODO: als Ergebnisknoten (blatt) markieren
 *
 * @author Holger Albert
 */
public class WspmResultQIntervalNode extends AbstractWspmResultNode implements ITuhhCalculationNode
{
  /**
   * The path of the q interval file.
   */
  private final IPath m_qIntervalPath;

  /**
   * The label.
   */
  private final String m_label;

  /**
   * The constructor.
   *
   * @param parentNode
   *          The parent node.
   * @param qIntervalPath
   *          The path of the q interval file.
   * @param label
   *          The label.
   */
  public WspmResultQIntervalNode( final IWspmResultNode parentNode, final IPath qIntervalPath, final String label )
  {
    super( parentNode );

    m_qIntervalPath = qIntervalPath;
    m_label = label;
  }

  @Override
  public String getLabel( )
  {
    return Messages.getString( "WspmResultQIntervalNode_0" ); //$NON-NLS-1$
  }

  @Override
  protected IWspmResultNode[] createChildren( )
  {
    return new IWspmResultNode[] {};
  }

  @Override
  public Object getObject( )
  {
    return m_qIntervalPath;
  }

  @Override
  protected String getInternalName( )
  {
    return m_label;
  }

  @Override
  public TuhhCalculation getCalculation( )
  {
    final IWspmResultNode parent = getParent();
    if( parent instanceof ITuhhCalculationNode )
      return ((ITuhhCalculationNode) parent).getCalculation();

    return null;
  }

  @Override
  public String toString( )
  {
    return String.format( "%s\n%s", super.toString(), m_qIntervalPath.toOSString() ); //$NON-NLS-1$
  }
}