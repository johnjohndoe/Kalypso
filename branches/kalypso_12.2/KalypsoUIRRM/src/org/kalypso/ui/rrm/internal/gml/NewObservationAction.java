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
package org.kalypso.ui.rrm.internal.gml;

import java.util.Iterator;

import org.apache.commons.lang3.StringUtils;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.jface.window.Window;
import org.eclipse.swt.widgets.Shell;
import org.kalypso.contribs.java.util.ValueIterator;
import org.kalypso.ogc.gml.typehandler.ZmlInlineTypeHandler;
import org.kalypso.ogc.sensor.IAxis;
import org.kalypso.ogc.sensor.ITupleModel;
import org.kalypso.ogc.sensor.impl.SimpleObservation;
import org.kalypso.ogc.sensor.impl.SimpleTupleModel;
import org.kalypso.ogc.sensor.metadata.MetadataList;
import org.kalypso.ogc.sensor.view.observationDialog.AbstractObservationAction;
import org.kalypso.ogc.sensor.view.observationDialog.AxisRangeDialog;
import org.kalypso.ogc.sensor.view.observationDialog.ObservationViewer;
import org.kalypso.ui.rrm.internal.i18n.Messages;

/**
 * @author Gernot Belger
 */
public class NewObservationAction extends AbstractObservationAction
{
  private final ZmlInlineTypeHandler m_typeHandler;

  public NewObservationAction( final ZmlInlineTypeHandler typeHandler )
  {
    m_typeHandler = typeHandler;
  }

  @Override
  protected String getLabel( )
  {
    return Messages.getString( "org.kalypso.ogc.sensor.view.ObservationViewerDialog.3" ); //$NON-NLS-1$ 
  }

  @Override
  protected String getTooltip( )
  {
    return Messages.getString( "org.kalypso.ogc.sensor.view.ObservationViewerDialog.4" ); //$NON-NLS-1$ 
  }

  @Override
  protected IStatus execute( )
  {
    final ObservationViewer viewer = getViewer();
    final Shell shell = viewer.getShell();

    final IAxis[] axis = m_typeHandler.createAxes();

    final AxisRangeDialog dialog = new AxisRangeDialog( shell, axis[0].getType() );
    if( dialog.open() != Window.OK )
      return Status.CANCEL_STATUS;

    if( !dialog.isValid() )
      return Status.OK_STATUS;

    final Object min = dialog.getMin();
    final Object intervall = dialog.getInt();
    final int rows = dialog.getCount();


    final Object[][] values = new Object[rows][axis.length];
    final Iterator< ? > iterator = new ValueIterator( min, intervall, rows );
    for( int row = 0; row < rows; row++ )
    {
      values[row][0] = iterator.next();
      for( int ax = 1; ax < axis.length; ax++ )
        values[row][ax] = dialog.getDefault();
    }
    final ITupleModel model = new SimpleTupleModel( axis, values );
    viewer.setInput( new SimpleObservation( null, StringUtils.EMPTY, new MetadataList(), model ), viewer.getShow() );

    return Status.OK_STATUS;
  }
}