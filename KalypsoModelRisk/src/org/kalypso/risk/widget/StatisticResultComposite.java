/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 *
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestraße 22
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
package org.kalypso.risk.widget;


import org.eclipse.jface.layout.GridLayoutFactory;
import org.eclipse.jface.viewers.TableViewer;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Table;
import org.kalypso.contribs.eclipse.swt.custom.ExcelTableCursor;
import org.kalypso.contribs.eclipse.swt.custom.ExcelTableCursor.ADVANCE_MODE;
import org.kalypso.observation.IObservation;
import org.kalypso.observation.result.TupleResult;
import org.kalypso.ogc.gml.om.ObservationFeatureFactory;
import org.kalypso.ogc.gml.om.table.TupleResultContentProvider2;
import org.kalypso.ogc.gml.om.table.handlers.IComponentUiHandlerProvider;
import org.kalypso.risk.model.schema.binding.IRasterizationControlModel;
import org.kalypsodeegree.model.feature.Feature;

/**
 * Composite which is showing the statistic result of a flood risk calculation
 *
 * @author Dirk Kuch
 */
public class StatisticResultComposite extends Composite
{
  private TableViewer m_viewer;

  public StatisticResultComposite( final IRasterizationControlModel model, final Composite parent, final int style )
  {
    super( parent, style );

    GridLayoutFactory.fillDefaults().applyTo( this );

    createControl( model );
  }

  private void createControl( final IRasterizationControlModel model )
  {
    final IComponentUiHandlerProvider provider = new StatisticResultComponentProvider();

    final Table table = new Table( this, SWT.BORDER | SWT.FULL_SELECTION | SWT.MULTI );
    table.setLayoutData( new GridData( SWT.FILL, SWT.FILL, true, true ) );

    table.setHeaderVisible( true );
    table.setLinesVisible( true );

    m_viewer = new TableViewer( table );
    m_viewer.setUseHashlookup( true );

    m_viewer.setContentProvider( new TupleResultContentProvider2( provider ) );

    final Feature observation = model.getStatisticObsFeature();
    final IObservation<TupleResult> obs = observation == null ? null : ObservationFeatureFactory.toObservation( observation );
    final TupleResult tupleResult = obs == null ? null : obs.getResult();

    m_viewer.setInput( tupleResult );

    final ExcelTableCursor cursor = new ExcelTableCursor( m_viewer, SWT.BORDER_DASH, ADVANCE_MODE.DOWN, true );
    cursor.setVisible( true );
    cursor.setEnabled( true );
  }

  public TableViewer getTableViewer( )
  {
    return m_viewer;
  }
}
