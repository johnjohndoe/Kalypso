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

import java.util.LinkedHashMap;
import java.util.Map;

import javax.xml.namespace.QName;

import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Table;
import org.eclipse.ui.forms.widgets.FormToolkit;
import org.kalypso.contribs.eclipse.jface.viewers.DefaultTableViewer;
import org.kalypso.model.wspm.core.IWspmConstants;
import org.kalypso.observation.IObservation;
import org.kalypso.observation.result.IComponent;
import org.kalypso.observation.result.TupleResult;
import org.kalypso.ogc.gml.om.ObservationFeatureFactory;
import org.kalypso.ogc.gml.om.table.TupleResultContentProvider;
import org.kalypso.ogc.gml.om.table.TupleResultLabelProvider;
import org.kalypso.ogc.gml.om.table.handlers.ComponentUiDecimalHandler;
import org.kalypso.ogc.gml.om.table.handlers.ComponentUiDoubleHandler;
import org.kalypso.ogc.gml.om.table.handlers.ComponentUiStringHandler;
import org.kalypso.ogc.gml.om.table.handlers.IComponentUiHandler;
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

  public StatisticResultComposite( final IRasterizationControlModel model, final Composite parent, final int style )
  {
    super( parent, style );

    paint( parent, model );
  }

  private void paint( final Composite parent, final IRasterizationControlModel model )
  {
    final FormToolkit toolkit = new FormToolkit( getDisplay() );

    final IComponentUiHandlerProvider provider = new IComponentUiHandlerProvider()
    {
      public Map<Integer, IComponentUiHandler> createComponentHandler( TupleResult tupleResult )
      {
        Map<Integer, IComponentUiHandler> myMap = new LinkedHashMap<Integer, IComponentUiHandler>();

        IComponent[] components = tupleResult.getComponents();

        int count = 0;
        for( IComponent component : components )
        {
          QName valueTypeName = component.getValueTypeName();
          if( valueTypeName.equals( IWspmConstants.Q_STRING ) )
            myMap.put( count, new ComponentUiStringHandler( count, false, true, false, component.getName(), SWT.NONE, 200, 30, "%s", "", "" ) );
          else if( valueTypeName.equals( IWspmConstants.Q_DECIMAL ) )
            myMap.put( count, new ComponentUiDecimalHandler( count, false, true, false, component.getName(), SWT.RIGHT, 100, 10, "%.02f", "", "%.02f" ) );
          else if( valueTypeName.equals( IWspmConstants.Q_DOUBLE ) )
            myMap.put( count, new ComponentUiDoubleHandler( count, false, true, false, component.getName(), SWT.RIGHT, 100, 10, "%.02f", "", "%.02f" ) );

          count++;
        }

        return myMap;
      }

    };

    DefaultTableViewer viewer = new DefaultTableViewer( this, SWT.BORDER );

    final Table table = viewer.getTable();
    table.setLayoutData( new GridData( SWT.FILL, SWT.FILL, true, true ) );
    table.setHeaderVisible( true );
    table.setLinesVisible( true );

    TupleResultContentProvider tupleResultContentProvider = new TupleResultContentProvider( provider );
    TupleResultLabelProvider tupleResultLabelProvider = new TupleResultLabelProvider( tupleResultContentProvider );

    viewer.setContentProvider( tupleResultContentProvider );
    viewer.setLabelProvider( tupleResultLabelProvider );

    final Feature observation = model.getStatisticObsFeature();
    final IObservation<TupleResult> obs = observation == null ? null : ObservationFeatureFactory.toObservation( observation );
    final TupleResult tupleResult = obs == null ? null : obs.getResult();

    viewer.setInput( tupleResult );

    toolkit.adapt( this );
  }
}
