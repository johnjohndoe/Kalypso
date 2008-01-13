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
package org.kalypso.ogc.gml.om.table;

import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.ui.part.ViewPart;
import org.kalypso.contribs.eclipse.jface.viewers.DefaultTableViewer;
import org.kalypso.core.KalypsoCorePlugin;
import org.kalypso.observation.IObservation;
import org.kalypso.observation.result.TupleResult;
import org.kalypso.ogc.gml.om.ObservationFeatureFactory;
import org.kalypso.ogc.gml.om.table.handlers.IComponentUiHandler;
import org.kalypso.ogc.gml.selection.FeatureSelectionHelper;
import org.kalypso.ogc.gml.selection.IFeatureSelection;
import org.kalypso.ogc.gml.selection.IFeatureSelectionListener;
import org.kalypsodeegree.model.feature.Feature;

/**
 * @author schlienger
 */
public class ObservationTableView extends ViewPart implements IFeatureSelectionListener
{
  protected DefaultTableViewer m_viewer;

  private TupleResultContentProvider m_tupleResultContentProvider;

  private TupleResultLabelProvider m_tupleResultLabelProvider;

  /**
   * @see org.eclipse.ui.part.WorkbenchPart#createPartControl(org.eclipse.swt.widgets.Composite)
   */
  @Override
  public void createPartControl( final Composite parent )
  {
    KalypsoCorePlugin.getDefault().getSelectionManager().addSelectionListener( this );

    m_viewer = new DefaultTableViewer( parent, SWT.BORDER | SWT.MULTI | SWT.FULL_SELECTION );
    m_viewer.getTable().setHeaderVisible( true );
    m_viewer.getTable().setLinesVisible( true );

    final IComponentUiHandler[] handler = new IComponentUiHandler[] {};
    m_tupleResultContentProvider = new TupleResultContentProvider( handler );
    m_viewer.setContentProvider( m_tupleResultContentProvider );
    m_tupleResultLabelProvider = new TupleResultLabelProvider( handler );
    m_viewer.setLabelProvider( m_tupleResultLabelProvider );
    m_viewer.setCellModifier( new TupleResultCellModifier( m_tupleResultContentProvider ) );

    selectionChanged( KalypsoCorePlugin.getDefault().getSelectionManager() );
  }

  /**
   * @see org.eclipse.ui.part.WorkbenchPart#dispose()
   */
  @Override
  public void dispose( )
  {
    KalypsoCorePlugin.getDefault().getSelectionManager().removeSelectionListener( this );

    m_tupleResultContentProvider.dispose();
    m_tupleResultLabelProvider.dispose();

    super.dispose();
  }

  /**
   * @see org.eclipse.ui.part.WorkbenchPart#setFocus()
   */
  @Override
  public void setFocus( )
  {
    m_viewer.getControl().setFocus();
  }

  /**
   * @see org.kalypso.ogc.gml.selection.IFeatureSelectionListener#selectionChanged(org.kalypso.ogc.gml.selection.IFeatureSelection)
   */
  public void selectionChanged( final IFeatureSelection selection )
  {
    m_viewer.setInput( null );

    final Feature[] obsFeatures = FeatureSelectionHelper.getAllFeaturesOfType( selection, ObservationFeatureFactory.OM_OBSERVATION );

    if( obsFeatures.length > 0 )
    {
      final IObservation<TupleResult> obs = ObservationFeatureFactory.toObservation( obsFeatures[0] );

      m_viewer.setInput( obs.getResult() );
    }
  }
}
