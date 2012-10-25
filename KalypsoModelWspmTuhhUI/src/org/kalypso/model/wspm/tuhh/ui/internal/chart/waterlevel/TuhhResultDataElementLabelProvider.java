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
package org.kalypso.model.wspm.tuhh.ui.internal.chart.waterlevel;

import org.eclipse.jface.viewers.LabelProvider;
import org.eclipse.swt.graphics.Image;
import org.kalypso.model.wspm.tuhh.core.results.IWspmResultNode;
import org.kalypso.model.wspm.tuhh.core.results.WspmResultLabelProvider;

/**
 * @author Gernot Belger
 */
class TuhhResultDataElementLabelProvider extends LabelProvider
{
  private final WspmResultLabelProvider m_delegate = new WspmResultLabelProvider( null );

  @Override
  public void dispose( )
  {
    m_delegate.dispose();

    super.dispose();
  }

  @Override
  public String getText( final Object element )
  {
    final TuhhResultDataElement dataElement = (TuhhResultDataElement)element;
    return dataElement.getLabel();
  }

  @Override
  public Image getImage( final Object element )
  {
    final TuhhResultDataElement dataElement = (TuhhResultDataElement)element;

    final IWspmResultNode node = dataElement.getResultNode();
    if( node == null )
    {
      // FIXME: 2d waterlevle, return waterlevel image
    }

    return m_delegate.getImage( node );
  }
}