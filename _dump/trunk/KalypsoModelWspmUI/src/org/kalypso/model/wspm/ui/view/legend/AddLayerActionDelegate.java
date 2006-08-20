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
import java.util.Collection;
import java.util.LinkedList;

import org.eclipse.jface.action.IAction;
import org.eclipse.jface.viewers.ArrayContentProvider;
import org.eclipse.jface.viewers.LabelProvider;
import org.eclipse.ui.dialogs.ListDialog;
import org.kalypso.model.wspm.core.profil.IProfil;
import org.kalypso.model.wspm.core.profil.IProfilBuilding;
import org.kalypso.model.wspm.core.profil.IProfilChange;
import org.kalypso.model.wspm.core.profil.IProfilEventManager;
import org.kalypso.model.wspm.core.profil.ProfilBuildingFactory;
import org.kalypso.model.wspm.core.profil.ProfilDataException;
import org.kalypso.model.wspm.core.profil.IProfilBuilding.BUILDING_TYP;
import org.kalypso.model.wspm.core.profil.IProfilPoint.POINT_PROPERTY;
import org.kalypso.model.wspm.core.profil.changes.BuildingSet;
import org.kalypso.model.wspm.core.profil.changes.PointPropertyAdd;
import org.kalypso.model.wspm.ui.profil.operation.ProfilOperation;
import org.kalypso.model.wspm.ui.profil.operation.ProfilOperationJob;
import org.kalypso.model.wspm.ui.profil.view.chart.ProfilChartView;


public class AddLayerActionDelegate extends AbstractLegendViewActionDelegate
{
  public void run( final IAction action )
  {
    // welche layer-typen können hinzugefügt werden?
    
    final ProfilChartView profilChartView = getView().getProfilChartView();
    final IProfil profil = profilChartView.getProfil();
    if( profil == null )
    {
      handleError( "Es wird gerade kein Profil editiert." );
      return;
    }

    // liste anzeigen
    final AddableLayer[] addables = createAddables( profil );
    final ListDialog dialog = new ListDialog( getView().getSite().getShell() );
    dialog.setAddCancelButton( true );
    dialog.setBlockOnOpen( true );
    dialog.setContentProvider( new ArrayContentProvider() );
    dialog.setLabelProvider( new LabelProvider() );
    dialog.setInput( addables );
    dialog.setMessage( "Folgende Datensätze können hinzugefügt werden:" );
    dialog.setTitle( "Datensatz hinzufügen" );

    dialog.open();

    final Object[] result = dialog.getResult();
    if( result == null || result.length != 1 )
      return;

    // TODO: reset undo + message to user
    try
    {
      ((AddableLayer)result[0]).perform( profil, profilChartView.getProfilEventManager() );
    }
    catch( final ProfilDataException e )
    {
      e.printStackTrace();

      handleError( "Datensatz konnte nicht hinzugefügt werden.\n" + e.getLocalizedMessage() );
    }
  }

  private AddableLayer[] createAddables( final IProfil profil )
  {
    final Collection<AddableLayer> addables = new ArrayList<AddableLayer>();

    // Bauwerke

    if( profil.getBuilding() == null )
    {
      for( AddableLayer bl : AddableBuildingLayer.getAddableBuildingLayer() )
      {
        addables.add( bl );
      }
    }

    final LinkedList<POINT_PROPERTY> pointProperties = profil.getPointProperties( false );

    // Bewuchs
    if( !pointProperties.contains( POINT_PROPERTY.BEWUCHS_AX ) )
      addables.add( AddableLayer.BEWUCHS );

    // Rechtswert Hochwert
    if( !pointProperties.contains( POINT_PROPERTY.HOCHWERT ) )
      addables.add( AddableLayer.HOCHWERT );
    
//  Rauheit
    if( !pointProperties.contains( POINT_PROPERTY.RAUHEIT ) )
      addables.add( AddableLayer.RAUHEIT );    
    
    return addables.toArray( new AddableLayer[addables.size()] );
  }

  private static abstract class AddableLayer
  {
    public static final AddableLayer BEWUCHS = new AddableLayer( "Bewuchs" )
    {
      @Override
      public void perform( final IProfil profil, final IProfilEventManager pem )
      {
        final IProfilChange[] changes = new IProfilChange[3];
        changes[0] = new PointPropertyAdd(profil,POINT_PROPERTY.BEWUCHS_AX,0 );
        changes[1] = new PointPropertyAdd(profil,POINT_PROPERTY.BEWUCHS_AY,0 );
        changes[2] = new PointPropertyAdd(profil,POINT_PROPERTY.BEWUCHS_DP,0 );

        final ProfilOperation operation = new ProfilOperation( "Bewuchs einfügen", pem,
            changes, true );
        new ProfilOperationJob( operation ).schedule();
      }
    };

    public static final AddableLayer HOCHWERT = new AddableLayer( "Geokoordinaten RW/HW" )
    {
      @Override
      public void perform( final IProfil profil, final IProfilEventManager pem )
      {
        final IProfilChange[] changes = new IProfilChange[2];
        changes[0] = new PointPropertyAdd(profil,POINT_PROPERTY.HOCHWERT,0 );
        changes[1] = new PointPropertyAdd(profil,POINT_PROPERTY.RECHTSWERT,0 );
        final ProfilOperation operation = new ProfilOperation( "Geokoordinaten einfügen", pem,
            changes, true );
        new ProfilOperationJob( operation ).schedule();
      }
    };
    
    
    public static final AddableLayer RAUHEIT = new AddableLayer( "Rauheiten" )
    {
      @Override
      public void perform( final IProfil profil, final IProfilEventManager pem )
      {
        final IProfilChange change = new PointPropertyAdd(profil,POINT_PROPERTY.RAUHEIT,0 );
        final ProfilOperation operation = new ProfilOperation( "Rauheiten einfügen", pem,
            change, true );
        new ProfilOperationJob( operation ).schedule();
      }
    };
    private final String m_label;

    private AddableLayer( final String label )
    {
      m_label = label;
    }

    public abstract void perform( final IProfil profil, final IProfilEventManager pem )
        throws ProfilDataException;

    @Override
    public String toString( )
    {
      return m_label;
    }
  }

  private static class AddableBuildingLayer extends AddableLayer
  {

    public final static AddableLayer BRUECKE = new AddableBuildingLayer( "Brücke",
        BUILDING_TYP.BRUECKE );

    public final static AddableLayer WEHR = new AddableBuildingLayer( "Wehr", BUILDING_TYP.WEHR );

    public final static AddableLayer KREIS = new AddableBuildingLayer( "Kreis - Durchlass",
        BUILDING_TYP.KREIS );

    public final static AddableLayer TRAPEZ = new AddableBuildingLayer( "Trapez - Durchlass",
        BUILDING_TYP.TRAPEZ );

    public final static AddableLayer MAUL = new AddableBuildingLayer( "Maul - Durchlass",
        BUILDING_TYP.MAUL );

    public final static AddableLayer EI = new AddableBuildingLayer( "Ei - Durchlass",
        BUILDING_TYP.EI );

    private final BUILDING_TYP m_buildingTyp;

    private AddableBuildingLayer( final String label, final BUILDING_TYP buildingTyp )
    {
      super( label );

      m_buildingTyp = buildingTyp;
    }

    final static AddableLayer[] getAddableBuildingLayer( )
    {
      AddableLayer[] buildingLayer =
      { BRUECKE, WEHR, KREIS, TRAPEZ, MAUL, EI };
      return buildingLayer;
    }

    @Override
    public void perform( final IProfil profil, final IProfilEventManager pem )
    {
      final IProfilBuilding building = ProfilBuildingFactory.createProfilBuilding( m_buildingTyp );
      
      final BuildingSet buildingChange = new BuildingSet( profil, building );
      final ProfilOperation operation = new ProfilOperation( building.toString()+" einfügen", pem,
          buildingChange, true );
      new ProfilOperationJob( operation ).schedule();

    }
  }
}