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
package org.kalypso.kalypsomodel1d2d.ui.map.del;

import java.awt.event.KeyEvent;
import java.util.List;

import org.kalypso.afgui.KalypsoAFGUIFrameworkPlugin;
import org.kalypso.afgui.scenarios.SzenarioDataProvider;
import org.kalypso.contribs.eclipse.swt.awt.SWT_AWT_Utilities;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFE1D2DComplexElement;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFEDiscretisationModel1d2d;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFELine;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IJunctionElement;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.ITransitionElement;
import org.kalypso.kalypsomodel1d2d.schema.binding.flowrel.IBoundaryCondition;
import org.kalypso.kalypsosimulationmodel.core.flowrel.IFlowRelationshipModel;
import org.kalypso.ogc.gml.map.MapPanel;
import org.kalypso.ogc.gml.map.widgets.SingleElementSelectWidget;
import org.kalypso.ogc.gml.selection.EasyFeatureWrapper;
import org.kalypso.ogc.gml.selection.IFeatureSelectionManager;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureList;
import org.kalypsodeegree.model.feature.binding.IFeatureWrapperCollection;

public class DeleteFEContlineWidget extends SingleElementSelectWidget
{
  public DeleteFEContlineWidget( )
  {
    super( Messages.getString("DeleteFEContlineWidget.0"), Messages.getString("DeleteFEContlineWidget.1") ); //$NON-NLS-1$ //$NON-NLS-2$
  }

  /**
   * @see org.kalypso.ogc.gml.map.widgets.AbstractSelectWidget#keyReleased(java.awt.event.KeyEvent)
   */
  @Override
  public void keyReleased( final KeyEvent event )
  {
    final MapPanel mapPanel = getMapPanel();
    final IFeatureSelectionManager selectionManager = mapPanel.getSelectionManager();
    final EasyFeatureWrapper[] selected = selectionManager.getAllFeatures();
    if( selected.length == 0 )
      return;

    if( !SWT_AWT_Utilities.showSwtMessageBoxConfirm( Messages.getString("DeleteFEContlineWidget.2"), Messages.getString("DeleteFEContlineWidget.3") ) ) //$NON-NLS-1$ //$NON-NLS-2$
      return;

    // selectionManager.clear(); active selection is still needed for super.keyReleased( event );

    try
    {
      // to allow continuity line to be deleted, no boundary conditions cannot be positioned on that line
      // also, this line cannot be a part of any transition or junction element

      final SzenarioDataProvider dataProvider = KalypsoAFGUIFrameworkPlugin.getDefault().getDataProvider();
      final IFEDiscretisationModel1d2d discretisationModel = dataProvider.getModel( IFEDiscretisationModel1d2d.class );
      final IFlowRelationshipModel flowRelationshipModel = dataProvider.getModel( IFlowRelationshipModel.class );

      final IFeatureWrapperCollection<IFE1D2DComplexElement> complexElements = discretisationModel.getComplexElements();
      for( final IFE1D2DComplexElement complexElement : complexElements )
      {
        if( complexElement instanceof ITransitionElement )
        {
          final ITransitionElement transitionElement = (ITransitionElement) complexElement;
          final List<IFELine> continuityLines = transitionElement.getContinuityLines();
          for( final IFELine line : continuityLines )
          {
            for( int i = 0; i < selected.length; i++ )
              if( line.getGmlID().equals( selected[i].getFeature().getId() ) )
              {
                SWT_AWT_Utilities.showSwtMessageBoxInformation( Messages.getString("DeleteFEContlineWidget.4"), Messages.getString("DeleteFEContlineWidget.5") ); //$NON-NLS-1$ //$NON-NLS-2$
                selectionManager.clear();
                return;
              }
          }
        }
        if( complexElement instanceof IJunctionElement )
        {
          final IJunctionElement junctionElement = (IJunctionElement) complexElement;
          final List<IFELine> continuityLines = junctionElement.getContinuityLines();
          for( final IFELine line : continuityLines )
          {
            for( int i = 0; i < selected.length; i++ )
              if( line.getGmlID().equals( selected[i].getFeature().getId() ) )
              {
                SWT_AWT_Utilities.showSwtMessageBoxInformation( Messages.getString("DeleteFEContlineWidget.6"), Messages.getString("DeleteFEContlineWidget.7") ); //$NON-NLS-1$ //$NON-NLS-2$
                selectionManager.clear();
                return;
              }
          }
        }
      }

      final FeatureList wrappedList = flowRelationshipModel.getWrappedList();
      for( final Object object : wrappedList )
      {
        final IBoundaryCondition bc = (IBoundaryCondition) ((Feature) object).getAdapter( IBoundaryCondition.class );
        if( bc != null )
        {
          final String parentElementID = bc.getParentElementID();
          for( int i = 0; i < selected.length; i++ )
            if( selected[i].getFeature().getId().equals( parentElementID ) )
            {
              SWT_AWT_Utilities.showSwtMessageBoxInformation( Messages.getString("DeleteFEContlineWidget.8"), Messages.getString("DeleteFEContlineWidget.9") ); //$NON-NLS-1$ //$NON-NLS-2$
              selectionManager.clear();
              return;
            }
        }
      }

      super.keyReleased( event );
    }
    catch( final Exception e )
    {
      e.printStackTrace();
      throw new RuntimeException( e.getMessage(), e );
    }

  }
}
