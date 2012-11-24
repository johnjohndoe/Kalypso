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
package org.kalypso.model.wspm.pdb.ui.internal.content;

import org.apache.commons.lang3.ObjectUtils;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.jface.viewers.ColumnLabelProvider;
import org.eclipse.swt.graphics.Image;
import org.kalypso.core.status.StatusComposite;
import org.kalypso.model.wspm.pdb.db.constants.EventConstants.TYPE;
import org.kalypso.model.wspm.pdb.db.mapping.CrossSection;
import org.kalypso.model.wspm.pdb.db.mapping.Event;
import org.kalypso.model.wspm.pdb.db.mapping.State;
import org.kalypso.model.wspm.pdb.db.mapping.WaterBody;
import org.kalypso.model.wspm.pdb.ui.internal.WspmPdbUiImages;
import org.kalypso.model.wspm.pdb.ui.internal.WspmPdbUiImages.IMAGE;
import org.kalypso.model.wspm.pdb.ui.internal.i18n.Messages;

/**
 * @author Gernot Belger
 */
public class PdbLabelProvider extends ColumnLabelProvider
{
  public static final String PENDING = Messages.getString( "PdbLabelProvider.0" ); //$NON-NLS-1$

  public static final String EMPTY_STATES = Messages.getString( "PdbLabelProvider.1" ); //$NON-NLS-1$

  public static final Object EMPTY_WATER_BODY = Messages.getString( "PdbLabelProvider.2" ); //$NON-NLS-1$

  @Override
  public String getText( final Object element )
  {
    if( element instanceof WaterBody )
      return ((WaterBody)element).getLabel();

    if( element instanceof State )
      return ((State)element).getName();

    if( element instanceof Event )
      return ((Event)element).getName();

    if( element instanceof CrossSection )
      return ObjectUtils.toString( ((CrossSection)element).getStation() );

    if( element instanceof IStatus )
      return ((IStatus)element).getMessage();

    return super.getText( element );
  }

  @Override
  public Image getImage( final Object element )
  {
    if( element instanceof WaterBody )
      return WspmPdbUiImages.getImage( IMAGE.WATER_BODY );

    if( element instanceof State )
      return WspmPdbUiImages.getImage( IMAGE.STATE );

    if( element instanceof Event )
    {
      final TYPE type = ((Event)element).getType();
      switch( type )
      {
        case Simulation:
          return WspmPdbUiImages.getImage( IMAGE.EVENT_SIMULATION );

        case Measurement:
        default:
          return WspmPdbUiImages.getImage( IMAGE.EVENT_MEASURED );
      }
    }

    if( element instanceof CrossSection )
      return WspmPdbUiImages.getImage( IMAGE.CROSS_SECTION );

    if( element instanceof IStatus )
      return StatusComposite.getStatusImage( (IStatus)element );

    if( element == PENDING )
      return WspmPdbUiImages.getImage( IMAGE.PENDING );

    return super.getImage( element );
  }
}