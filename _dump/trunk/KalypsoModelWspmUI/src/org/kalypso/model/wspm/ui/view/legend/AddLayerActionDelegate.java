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
package org.kalypso.model.wspm.ui.view.legend;

import java.util.ArrayList;
import java.util.List;

import org.eclipse.jface.action.IAction;
import org.eclipse.jface.viewers.ArrayContentProvider;
import org.eclipse.jface.viewers.LabelProvider;
import org.eclipse.ui.dialogs.ListDialog;
import org.kalypso.model.wspm.core.profil.IProfil;
import org.kalypso.model.wspm.ui.KalypsoModelWspmUIExtensions;
import org.kalypso.model.wspm.ui.Messages;
import org.kalypso.model.wspm.ui.view.chart.IProfilChartLayer;
import org.kalypso.model.wspm.ui.view.chart.IProfilLayerProvider;
import org.kalypso.model.wspm.ui.view.chart.ProfilChartView;

public class AddLayerActionDelegate extends AbstractLegendViewActionDelegate
{
  public void run( final IAction action )
  {
    // welche layer-typen können hinzugefügt werden?

    final ProfilChartView chartView = getView().getProfilChartView();
    final IProfil profil = chartView.getProfil();
    if( profil == null )
    {
      handleError( Messages.AddLayerActionDelegate_0 );
      return;
    }

    // liste anzeigen

    final List<IProfilChartLayer> addables = new ArrayList<IProfilChartLayer>();
    final IProfilLayerProvider layerProvider = KalypsoModelWspmUIExtensions.createProfilLayerProvider( profil.getType() );
    if( layerProvider != null )
    {
      for( final String al : layerProvider.getAddableLayers( chartView ) )
      {
        final IProfilChartLayer layer = layerProvider.createLayer( al, chartView );
        if( layer != null )
          addables.add( layer );
      }
    }

    final ListDialog dialog = new ListDialog( null);//getView().getSite().getShell() );
    dialog.setAddCancelButton( true );
    dialog.setBlockOnOpen( true );
    dialog.setContentProvider( new ArrayContentProvider() );
    dialog.setLabelProvider( new LabelProvider()
    {
      @Override
      public final String getText( Object element )
      {
        return ((IProfilChartLayer) element).getTitle();
      }
    } );
    dialog.setInput( addables );
    dialog.setMessage( Messages.AddLayerActionDelegate_1 );
    dialog.setTitle( Messages.AddLayerActionDelegate_2 );

    dialog.open();

    final Object[] result = dialog.getResult();
    if( result == null || result.length != 1 )
      return;
    // TODO: reset undo + message to user
    final String layerToAdd = ((IProfilChartLayer) result[0]).getId();

    if( layerProvider != null )
    {
      if( layerProvider.providesLayer( layerToAdd ) )
      {
        layerProvider.addLayerToChart( chartView, layerToAdd );
      }
    }
  }
}
