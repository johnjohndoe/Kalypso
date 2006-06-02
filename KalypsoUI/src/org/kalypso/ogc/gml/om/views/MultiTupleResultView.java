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
package org.kalypso.ogc.gml.om.views;

import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.Viewer;
import org.eclipse.jface.viewers.ViewerDropAdapter;
import org.eclipse.swt.SWT;
import org.eclipse.swt.dnd.DND;
import org.eclipse.swt.dnd.DropTarget;
import org.eclipse.swt.dnd.Transfer;
import org.eclipse.swt.dnd.TransferData;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.ui.part.ViewPart;
import org.eclipse.ui.views.navigator.LocalSelectionTransfer;
import org.kalypso.contribs.eclipse.jface.viewers.DefaultTableViewer;
import org.kalypso.gmlschema.GMLSchemaUtilities;
import org.kalypso.observation.IObservation;
import org.kalypso.observation.result.ComponentUtilities;
import org.kalypso.observation.result.IComponent;
import org.kalypso.observation.result.TupleResult;
import org.kalypso.observation.table.MultiTupleResultModel;
import org.kalypso.observation.table.TupleResultColumn;
import org.kalypso.ogc.gml.om.ObservationFeatureFactory;
import org.kalypso.ogc.gml.om.tableex.MultiTupleResultContentProvider;
import org.kalypso.ogc.gml.om.tableex.MultiTupleResultLabelProvider;
import org.kalypso.ogc.gml.selection.EasyFeatureWrapper;
import org.kalypso.ogc.gml.selection.IFeatureSelection;
import org.kalypsodeegree.model.feature.Feature;

/**
 * @author schlienger
 */
public class MultiTupleResultView extends ViewPart
{
  protected DefaultTableViewer m_viewer;

  private DropTarget m_dropTarget;

  protected final MultiTupleResultModel m_model = new MultiTupleResultModel();

  /**
   * @see org.eclipse.ui.part.WorkbenchPart#createPartControl(org.eclipse.swt.widgets.Composite)
   */
  @Override
  public void createPartControl( final Composite parent )
  {
    m_viewer = new DefaultTableViewer( parent, SWT.BORDER | SWT.MULTI | SWT.FULL_SELECTION );
    m_viewer.addDropSupport( DND.DROP_COPY | DND.DROP_MOVE | DND.DROP_LINK, new Transfer[] { LocalSelectionTransfer.getInstance() }, new DropAdapter( m_viewer ) );
    m_viewer.getTable().setHeaderVisible( true );
    m_viewer.getTable().setLinesVisible( true );

    final MultiTupleResultLabelProvider labelProvider = new MultiTupleResultLabelProvider();
    m_viewer.setContentProvider( new MultiTupleResultContentProvider( labelProvider ) );
    m_viewer.setLabelProvider( labelProvider );

    m_viewer.setInput( m_model );
  }

  /**
   * @see org.eclipse.ui.part.WorkbenchPart#dispose()
   */
  @Override
  public void dispose( )
  {
    if( m_dropTarget != null )
      m_dropTarget.dispose();

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
   * @author schlienger
   */
  public class DropAdapter extends ViewerDropAdapter
  {
    protected DropAdapter( Viewer viewer )
    {
      super( viewer );
    }

    /**
     * @see org.eclipse.jface.viewers.ViewerDropAdapter#performDrop(java.lang.Object)
     */
    @Override
    public boolean performDrop( Object data )
    {
      final ISelection selection = LocalSelectionTransfer.getInstance().getSelection();

      boolean success = false;

      if( selection instanceof IFeatureSelection )
      {
        final IFeatureSelection fSel = (IFeatureSelection) selection;
        final EasyFeatureWrapper[] features = fSel.getAllFeatures();

        for( int i = 0; i < features.length; i++ )
        {
          final Feature feature = features[i].getFeature();

          if( GMLSchemaUtilities.substitutes( feature.getFeatureType(), ObservationFeatureFactory.OM_OBSERVATION ) )
          {
            final IObservation<TupleResult> obs = ObservationFeatureFactory.toObservation( feature );

            final IComponent[] components = obs.getResult().getComponents();

            if( m_model.getKeyComponent() == null && components.length >= 2 )
            {
              m_model.addColumn( new TupleResultColumn( components[1].getPosition(), obs.getResult(), components[0], components[1] ) );
              success = true;
            }
            else if( m_model.getKeyComponent() != null )
            {
              final IComponent kc = ComponentUtilities.sameComponent( components, m_model.getKeyComponent() );
              final IComponent vc = ComponentUtilities.otherComponent( components, kc.getValueTypeName() );

              if( kc != null && vc != null )
              {
                m_model.addColumn( new TupleResultColumn( vc.getPosition(), obs.getResult(), kc, vc ) );
                success = true;
              }
            }
          }
        }
      }

      return success;
    }

    /**
     * @see org.eclipse.jface.viewers.ViewerDropAdapter#validateDrop(java.lang.Object, int,
     *      org.eclipse.swt.dnd.TransferData)
     */
    @Override
    public boolean validateDrop( Object target, int operation, TransferData transferType )
    {
      return true;
    }
  }
}
