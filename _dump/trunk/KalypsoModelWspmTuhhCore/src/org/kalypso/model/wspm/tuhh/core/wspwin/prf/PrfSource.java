/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 * 
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestra�e 22
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
package org.kalypso.model.wspm.tuhh.core.wspwin.prf;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.Reader;
import java.util.LinkedList;
import java.util.StringTokenizer;

import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.kalypso.commons.KalypsoCommonsPlugin;
import org.kalypso.commons.math.Range;
import org.kalypso.commons.math.geom.PolyLine;
import org.kalypso.model.wspm.core.KalypsoModelWspmCoreExtensions;
import org.kalypso.model.wspm.core.profil.IProfil;
import org.kalypso.model.wspm.core.profil.IProfilPointMarker;
import org.kalypso.model.wspm.core.profil.IProfilPointMarkerProvider;
import org.kalypso.model.wspm.core.profil.IProfilPointPropertyProvider;
import org.kalypso.model.wspm.core.profil.IProfileObject;
import org.kalypso.model.wspm.core.profil.serializer.IProfilSource;
import org.kalypso.model.wspm.core.profil.util.ProfilObsHelper;
import org.kalypso.model.wspm.core.profil.util.ProfilUtil;
import org.kalypso.model.wspm.tuhh.core.IWspmTuhhConstants;
import org.kalypso.model.wspm.tuhh.core.profile.ProfilDevider;
import org.kalypso.model.wspm.tuhh.core.profile.buildings.building.BuildingBruecke;
import org.kalypso.model.wspm.tuhh.core.profile.buildings.building.BuildingWehr;
import org.kalypso.model.wspm.tuhh.core.profile.buildings.durchlass.BuildingEi;
import org.kalypso.model.wspm.tuhh.core.profile.buildings.durchlass.BuildingKreis;
import org.kalypso.model.wspm.tuhh.core.profile.buildings.durchlass.BuildingMaul;
import org.kalypso.model.wspm.tuhh.core.profile.buildings.durchlass.BuildingTrapez;
import org.kalypso.observation.result.IComponent;
import org.kalypso.observation.result.IRecord;
import org.kalypso.wspwin.core.prf.PrfReader;
import org.kalypso.wspwin.core.prf.datablock.DataBlockHeader;
import org.kalypso.wspwin.core.prf.datablock.IDataBlock;

/**
 * @author kimwerner
 */
public class PrfSource implements IProfilSource
{

  private void readSource( final PrfReader pr, final IProfil p )
  {
    if( p == null )
      return;
    try
    {
      p.setProperty( "prfFileFormat_MetaData", pr.getMetaData() );
      p.setProperty( IWspmTuhhConstants.PROFIL_PROPERTY_STATUS, pr.getKeyValue( 3 )[1] );
      p.setProperty( IWspmTuhhConstants.PROFIL_PROPERTY_VERZWEIGUNGSKENNUNG, pr.getKeyValue( 5 )[1] );
      p.setProperty( IWspmTuhhConstants.PROFIL_PROPERTY_WASSERSPIEGEL, pr.getKeyValue( 6 )[1] );
      p.setProperty( IWspmTuhhConstants.PROFIL_PROPERTY_MEHRFELDBRUECKE, pr.getKeyValue( 7 )[1] );
      final String stat = pr.getKeyValue( 9 )[0];
      if( stat.startsWith( "STATION " ) )
        p.setStation( new Double( stat.substring( 10 ) ) );
      if( readPoints( p, pr ) > 0 )
      {
        readTrennFl( p, pr );
        readDurchStr( p, pr );
        readBordVoll( p, pr );
        readRauhheit( p, pr );
        readBewuchs( p, pr );
        readComment( p, pr );
        readGeoCoord( p, pr );
        if( !(readWehr( p, pr ) || readBridge( p, pr )) )
          readBuilding( p, pr );
      }
    }
    catch( final IllegalArgumentException e )
    {
      KalypsoCommonsPlugin.getDefault().getLog().log( new Status( IStatus.ERROR, "", 0, e.getMessage(), e ) );
    }
  }

  private void readComment( final IProfil p, final PrfReader pr )
  {
    final IDataBlock db = pr.getDataBlock( "KOM" );
    if( db == null )
      return;
    final StringBuffer sb = new StringBuffer();

    final String[] text = db.getText();
    for( int i = 0; i < text.length; i++ )
    {
      final String line = text[i];
      sb.append( line.substring( 3 ) );
      if( i != text.length - 1 )
        sb.append( System.getProperty( "line.separator" ) );
    }
    p.setComment( sb.toString() );
  }

  private void readBewuchs( final IProfil p, final PrfReader pr )
  {
    final IDataBlock dbx = pr.getDataBlock( "AX" );
    final IDataBlock dby = pr.getDataBlock( "AY" );
    final IDataBlock dbp = pr.getDataBlock( "DP" );
    if( dbx == null || dby == null || dbp == null )
      return;

    final IProfilPointPropertyProvider[] providers = KalypsoModelWspmCoreExtensions.getPointPropertyProviders( p.getType() );
    final IComponent prAx = ProfilObsHelper.getPointPropertyFromProviders( providers, IWspmTuhhConstants.POINT_PROPERTY_BEWUCHS_AX );
    final IComponent prAy = ProfilObsHelper.getPointPropertyFromProviders( providers, IWspmTuhhConstants.POINT_PROPERTY_BEWUCHS_AY );
    final IComponent prDp = ProfilObsHelper.getPointPropertyFromProviders( providers, IWspmTuhhConstants.POINT_PROPERTY_BEWUCHS_DP );

    writePointProperty( p, prAx, dbx );
    writePointProperty( p, prAy, dby );
    writePointProperty( p, prDp, dbp );
  }

  private void writePointProperty( final IProfil p, final IComponent property, final IDataBlock db )
  {
    p.addPointProperty( property );
    final double[] xs = db.getX();
    final double[] ys = db.getY();
    for( int i = 0; i < xs.length; i++ )
    {
      final IRecord point = ProfilUtil.findPoint( p, i, xs[i], 0 );
      if( point != null )
        point.setValue( property, ys[i] );
    }
  }

  private void readGeoCoord( final IProfil p, final PrfReader pr )
  {
    final IDataBlock dbh = pr.getDataBlock( "HOC" );
    final IDataBlock dbr = pr.getDataBlock( "REC" );
    if( dbh == null || dbr == null )
      return;

    final IProfilPointPropertyProvider[] providers = KalypsoModelWspmCoreExtensions.getPointPropertyProviders( p.getType() );
    final IComponent hochwert = ProfilObsHelper.getPointPropertyFromProviders( providers, IWspmTuhhConstants.POINT_PROPERTY_HOCHWERT );
    final IComponent rechtswert = ProfilObsHelper.getPointPropertyFromProviders( providers, IWspmTuhhConstants.POINT_PROPERTY_RECHTSWERT );

    writePointProperty( p, hochwert, dbh );
    writePointProperty( p, rechtswert, dbr );
  }

  private void readBuilding( final IProfil p, final PrfReader pr )
  {
    IDataBlock db = pr.getDataBlock( "EI" );
    if( db == null )
      db = pr.getDataBlock( "TRA" );
    if( db == null )
      db = pr.getDataBlock( "KRE" );
    if( db == null )
      db = pr.getDataBlock( "MAU" );
    if( db == null )
      return;
    final DataBlockHeader dbh = db.getDataBlockHeader();
    final String[] value = db.getText();
    if( value != null && value.length > 0 )
    {
      final StringTokenizer sT = new StringTokenizer( value[0], " " );
      double rauheit = 0.0;
      final IDataBlock dbRau = pr.getDataBlock( "RAU" );
      if( dbRau != null && dbRau.getY().length > 0 )
        rauheit = dbRau.getY()[0];
      switch( dbh.getSpecification( 8 ) )
      {
        case 6:// Trapez
        {
          final IProfileObject building = new BuildingTrapez( p );

          if( !writeBuildingProperty( building, sT, ProfilObsHelper.getPropertyFromId( building, IWspmTuhhConstants.BUILDING_PROPERTY_BREITE ) ) )
            return;
          if( !writeBuildingProperty( building, sT, ProfilObsHelper.getPropertyFromId( building, IWspmTuhhConstants.BUILDING_PROPERTY_HOEHE ) ) )
            return;
          if( !writeBuildingProperty( building, sT, ProfilObsHelper.getPropertyFromId( building, IWspmTuhhConstants.BUILDING_PROPERTY_STEIGUNG ) ) )
            return;
          if( !writeBuildingProperty( building, sT, ProfilObsHelper.getPropertyFromId( building, IWspmTuhhConstants.BUILDING_PROPERTY_SOHLGEFAELLE ) ) )
            return;
          if( !writeBuildingProperty( building, sT, ProfilObsHelper.getPropertyFromId( building, IWspmTuhhConstants.BUILDING_PROPERTY_BEZUGSPUNKT_X ) ) )
            return;
          if( !writeBuildingProperty( building, sT, ProfilObsHelper.getPropertyFromId( building, IWspmTuhhConstants.BUILDING_PROPERTY_BEZUGSPUNKT_Y ) ) )
            return;
          building.setValue( ProfilObsHelper.getPropertyFromId( p, IWspmTuhhConstants.BUILDING_PROPERTY_RAUHEIT ), rauheit );
          p.addProfileObjects( new IProfileObject[] { building } );
          break;
        }
        case 7:// Kreis
        {
          final IProfileObject building = new BuildingKreis( p );

          if( !writeBuildingProperty( building, sT, ProfilObsHelper.getPropertyFromId( building, IWspmTuhhConstants.BUILDING_PROPERTY_BREITE ) ) )
            return;
          if( !writeBuildingProperty( building, sT, ProfilObsHelper.getPropertyFromId( building, IWspmTuhhConstants.BUILDING_PROPERTY_SOHLGEFAELLE ) ) )
            return;
          if( !writeBuildingProperty( building, sT, ProfilObsHelper.getPropertyFromId( building, IWspmTuhhConstants.BUILDING_PROPERTY_BEZUGSPUNKT_X ) ) )
            return;
          if( !writeBuildingProperty( building, sT, ProfilObsHelper.getPropertyFromId( building, IWspmTuhhConstants.BUILDING_PROPERTY_BEZUGSPUNKT_Y ) ) )
            return;
          building.setValue( ProfilObsHelper.getPropertyFromId( building, IWspmTuhhConstants.BUILDING_PROPERTY_RAUHEIT ), rauheit );
          p.addProfileObjects( new IProfileObject[] { building } );
          break;
        }
        case 8:// Ei
        {
          final IProfileObject building = new BuildingEi( p );

          if( !writeBuildingProperty( building, sT, ProfilObsHelper.getPropertyFromId( building, IWspmTuhhConstants.BUILDING_PROPERTY_BREITE ) ) )
            return;
          if( !writeBuildingProperty( building, sT, ProfilObsHelper.getPropertyFromId( building, IWspmTuhhConstants.BUILDING_PROPERTY_HOEHE ) ) )
            return;
          if( !writeBuildingProperty( building, sT, ProfilObsHelper.getPropertyFromId( building, IWspmTuhhConstants.BUILDING_PROPERTY_SOHLGEFAELLE ) ) )
            return;
          if( !writeBuildingProperty( building, sT, ProfilObsHelper.getPropertyFromId( building, IWspmTuhhConstants.BUILDING_PROPERTY_BEZUGSPUNKT_X ) ) )
            return;
          if( !writeBuildingProperty( building, sT, ProfilObsHelper.getPropertyFromId( building, IWspmTuhhConstants.BUILDING_PROPERTY_BEZUGSPUNKT_Y ) ) )
            return;
          building.setValue( ProfilObsHelper.getPropertyFromId( building, IWspmTuhhConstants.BUILDING_PROPERTY_RAUHEIT ), rauheit );
          p.addProfileObjects( new IProfileObject[] { building } );
          break;
        }
        case 9:// Maulprofil
        {
          final IProfileObject building = new BuildingMaul( p );

          if( !writeBuildingProperty( building, sT, ProfilObsHelper.getPropertyFromId( building, IWspmTuhhConstants.BUILDING_PROPERTY_BREITE ) ) )
            return;
          if( !writeBuildingProperty( building, sT, ProfilObsHelper.getPropertyFromId( building, IWspmTuhhConstants.BUILDING_PROPERTY_HOEHE ) ) )
            return;
          if( !writeBuildingProperty( building, sT, ProfilObsHelper.getPropertyFromId( building, IWspmTuhhConstants.BUILDING_PROPERTY_SOHLGEFAELLE ) ) )
            return;
          if( !writeBuildingProperty( building, sT, ProfilObsHelper.getPropertyFromId( building, IWspmTuhhConstants.BUILDING_PROPERTY_BEZUGSPUNKT_X ) ) )
            return;
          if( !writeBuildingProperty( building, sT, ProfilObsHelper.getPropertyFromId( building, IWspmTuhhConstants.BUILDING_PROPERTY_BEZUGSPUNKT_Y ) ) )
            return;
          building.setValue( ProfilObsHelper.getPropertyFromId( building, IWspmTuhhConstants.BUILDING_PROPERTY_RAUHEIT ), rauheit );
          p.addProfileObjects( new IProfileObject[] { building } );
          break;
        }

      }

    }
  }

  private final boolean writeBuildingProperty( final IProfileObject building, final StringTokenizer sT, final IComponent property )
  {
    if( sT.hasMoreTokens() && (building != null) )
    {
      try
      {
        building.setValue( property, Double.parseDouble( sT.nextToken() ) );
        return true;
      }
      catch( final IllegalArgumentException e )
      {
        KalypsoCommonsPlugin.getDefault().getLog().log( new Status( IStatus.ERROR, "", 0, e.getMessage(), e ) );
        return false;
      }

    }
    else
      KalypsoCommonsPlugin.getDefault().getLog().log( new Status( IStatus.ERROR, "", 0, "Fehler beim Lesen der Bauwerkseigenschaft: " + property.toString(), null ) );
    return false;
  }

  private boolean readBridge( final IProfil p, final PrfReader pr )
  {
    final IDataBlock dbo = pr.getDataBlock( "OK-B" );
    final IDataBlock dbu = pr.getDataBlock( "UK-B" );
    if( dbo == null || dbu == null )
      return false;

    final IProfileObject bridge = new BuildingBruecke( p );
    final StringTokenizer sT = new StringTokenizer( dbu.getSecondLine(), " " );
    if( sT.countTokens() > 4 )
    {
      KalypsoCommonsPlugin.getDefault().getLog().log( new Status( IStatus.WARNING, "", 0, "Ung�ltige Anzahl von Eigenschaften f�r die Br�cke. Es werden nur die ersten Vier ausgewertet.", null ) );
    }

    final IComponent cUnterwasser = ProfilObsHelper.getPropertyFromId( bridge, IWspmTuhhConstants.BUILDING_PROPERTY_UNTERWASSER );
    final IComponent cBreite = ProfilObsHelper.getPropertyFromId( bridge, IWspmTuhhConstants.BUILDING_PROPERTY_BREITE );
    final IComponent cRauheit = ProfilObsHelper.getPropertyFromId( bridge, IWspmTuhhConstants.BUILDING_PROPERTY_RAUHEIT );
    final IComponent cFormbeiwert = ProfilObsHelper.getPropertyFromId( bridge, IWspmTuhhConstants.BUILDING_PROPERTY_FORMBEIWERT );

    writeBuildingProperty( bridge, sT, cUnterwasser );
    writeBuildingProperty( bridge, sT, cBreite );
    writeBuildingProperty( bridge, sT, cRauheit );
    writeBuildingProperty( bridge, sT, cFormbeiwert );

    final IComponent pp = ProfilObsHelper.getPropertyFromId( p, IWspmTuhhConstants.POINT_PROPERTY_OBERKANTEBRUECKE );
    final double delta = pp == null ? 0.0001 : ProfilObsHelper.getPrecision( pp );
    p.addProfileObjects( new IProfileObject[] { bridge } );
    final PolyLine polyLineO = new PolyLine( dbo.getX(), dbo.getY(), delta );
    final PolyLine polyLineU = new PolyLine( dbu.getX(), dbu.getY(), delta );
    final Range rangeO = new Range( polyLineO.getFirstX(), polyLineO.getLastX(), delta );
    final Range rangeU = new Range( polyLineU.getFirstX(), polyLineU.getLastX(), delta );

    for( final IRecord point : p.getPoints() )
    {
      final double breite = (Double) point.getValue( ProfilObsHelper.getPropertyFromId( point, IWspmTuhhConstants.POINT_PROPERTY_BREITE ) );
      final double hoehe = (Double) point.getValue( ProfilObsHelper.getPropertyFromId( point, IWspmTuhhConstants.POINT_PROPERTY_HOEHE ) );

      if( rangeO.contains( breite ) )
        point.setValue( ProfilObsHelper.getPropertyFromId( point, IWspmTuhhConstants.POINT_PROPERTY_OBERKANTEBRUECKE ), polyLineO.getYFor( breite ) );
      else
        point.setValue( ProfilObsHelper.getPropertyFromId( point, IWspmTuhhConstants.POINT_PROPERTY_OBERKANTEBRUECKE ), hoehe );
      if( rangeU.contains( breite ) )
        point.setValue( ProfilObsHelper.getPropertyFromId( point, IWspmTuhhConstants.POINT_PROPERTY_UNTERKANTEBRUECKE ), polyLineU.getYFor( breite ) );
      else
        point.setValue( ProfilObsHelper.getPropertyFromId( point, IWspmTuhhConstants.POINT_PROPERTY_UNTERKANTEBRUECKE ), hoehe );
    }
    return true;

  }

  private int readPoints( final IProfil p, final PrfReader pr )
  {
    final IDataBlock db = pr.getDataBlock( "Gelae" );
    if( db == null )
      return 0;
    final double[] xs = db.getX();
    final double[] ys = db.getY();
    p.addPointProperty( ProfilObsHelper.getPropertyFromId( p, IWspmTuhhConstants.POINT_PROPERTY_BREITE ) );
    p.addPointProperty( ProfilObsHelper.getPropertyFromId( p, IWspmTuhhConstants.POINT_PROPERTY_HOEHE ) );
    for( int i = 0; i < xs.length; i++ )
    {
      final IRecord point = p.createProfilPoint();
      point.setValue( ProfilObsHelper.getPropertyFromId( point, IWspmTuhhConstants.POINT_PROPERTY_BREITE ), xs[i] );
      point.setValue( ProfilObsHelper.getPropertyFromId( point, IWspmTuhhConstants.POINT_PROPERTY_HOEHE ), ys[i] );
      p.addPoint( point );
    }
    return xs.length;
  }

  private void readRauhheit( final IProfil p, final PrfReader pr )
  {
    final IDataBlock db = pr.getDataBlock( "RAU" );
    if( db == null )
      return;

    final IProfilPointPropertyProvider[] providers = KalypsoModelWspmCoreExtensions.getPointPropertyProviders( p.getType() );

    final String rks = db.getSecondLine().toUpperCase();
    IComponent rTyp = ProfilObsHelper.getPropertyFromId( p, IWspmTuhhConstants.POINT_PROPERTY_RAUHEIT_KS );

    if( rks.startsWith( "KST" ) )
    {
      rTyp = ProfilObsHelper.getPointPropertyFromProviders( providers, IWspmTuhhConstants.POINT_PROPERTY_RAUHEIT_KST );
    }
    else if( rks.startsWith( "KS" ) || rks.startsWith( "K-S " ) )
    {
      rTyp = ProfilObsHelper.getPointPropertyFromProviders( providers, IWspmTuhhConstants.POINT_PROPERTY_RAUHEIT_KS );
    }
    else
    {
      KalypsoCommonsPlugin.getDefault().getLog().log( new Status( IStatus.INFO, "", 0, "Unbekannter Rauheits-typ[" + rks + "], wird 'ks' interpretiert.", null ) );
    }

    p.addPointProperty( rTyp );
    for( int i = 0; i < db.getCoordCount(); i++ )
    {
      final IRecord point = ProfilUtil.findPoint( p, i, db.getX()[i], 0 );
      if( point != null )
        point.setValue( rTyp, db.getY()[i] );
    }
  }

  private void readTrennFl( final IProfil p, final PrfReader pr )
  {
    final LinkedList<IRecord> points = p.getPoints();
    if( points.isEmpty() )
      return;
    final IDataBlock db = pr.getDataBlock( "TRENNFLAECHEN" );
    if( db == null )
      return;
    final int pCount = db.getCoordCount();

    IRecord p1 = null;
    IRecord p2 = null;
    int pos1 = 0;
    int pos2 = 0;

    if( pCount > 0 )
    {
      p1 = ProfilUtil.findPoint( p, db.getX()[0], 0 );
      pos1 = (int) db.getY()[0];
    }
    if( pCount > 1 )
    {
      p2 = ProfilUtil.findPoint( p, db.getX()[1], 0 );
      pos2 = (int) db.getY()[1];
    }
    if( pCount > 2 )
      KalypsoCommonsPlugin.getDefault().getLog().log( new Status( IStatus.INFO, "", 0, "mehr als 2 Datens�tze f�r Trennfl�chen k�nnen an Station(" + p.getStation() + ") nicht ausgewertet werden", null ) );

    // TODO: KIM in den Reparator verschieben
    // --------------------
// if( p1 == null )
// {
// p1 = points.getFirst();
// pos1 = 0;
// m_logger.log( Level.INFO, "Erzeuge Trennfl�che f�r Station(" + p.getStation() + ") an Position [" + Double.toString(
// p1.getValueFor( IWspmTuhhConstants.POINT_PROPERTY_BREITE ) ) + "]" );
// }
// if( p2 == null )
// {
// p2 = points.getLast();
// pos2 = 0;
// m_logger.log( Level.INFO, "Erzeuge Trennfl�che f�r Station(" + p.getStation() + ") an Position [" + Double.toString(
// p2.getValueFor( IWspmTuhhConstants.POINT_PROPERTY_BREITE ) ) + "]" );
// }
    // --------------------

    final IProfilPointMarkerProvider[] providers = KalypsoModelWspmCoreExtensions.getMarkerProviders( p.getType() );
    if( providers.length != 1 )
      throw new IllegalStateException();

    final IProfilPointMarkerProvider provider = providers[0];

    if( p1 != null )
    {
      final IProfilPointMarker marker = provider.createProfilPointMarker( IWspmTuhhConstants.MARKER_TYP_TRENNFLAECHE, p1 );
      marker.setInterpretedValue( (pos1 == 3) );
    }

    if( p2 != null )
    {
      final IProfilPointMarker marker = provider.createProfilPointMarker( IWspmTuhhConstants.MARKER_TYP_TRENNFLAECHE, p2 );
      marker.setInterpretedValue( (pos2 == 4) );
    }
  }

  private void readDurchStr( final IProfil p, final PrfReader pr )
  {
    final LinkedList<IRecord> points = p.getPoints();
    if( points.isEmpty() )
      return;
    final IDataBlock db = pr.getDataBlock( "DURCHSTROEMTE" );
    if( db == null )
      return;
    final int pCount = db.getCoordCount();

    IRecord p1 = null;
    IRecord p2 = null;

    if( pCount > 0 )
    {
      p1 = ProfilUtil.findPoint( p, db.getX()[0], 0 );
    }
    if( pCount > 1 )
    {
      p2 = ProfilUtil.findPoint( p, db.getX()[1], 0 );
    }
    if( pCount > 2 )
      KalypsoCommonsPlugin.getDefault().getLog().log( new Status( IStatus.INFO, "", 0, "mehr als 2 Datens�tze an Station(" + p.getStation()
          + ")  f�r Durchstr�mte Bereiche k�nnen nicht ausgewertet werden", null ) );

    // TODO: KIM in den Reparator verschieben
    // -------------
// if( p1 == null )
// {
// p1 = points.getFirst();
// m_logger.log( Level.INFO, "Erzeuge Durchstr�mten Bereich f�r Station(" + p.getStation() + ") an Position [" +
// Double.toString( p1.getValueFor( IWspmTuhhConstants.POINT_PROPERTY_BREITE ) )
// + "]" );
// }
// if( p2 == null )
// {
// p2 = points.getLast();
// m_logger.log( Level.INFO, "Erzeuge Durchstr�mten Bereich f�r Station(" + p.getStation() + ") an Position [" +
// Double.toString( p2.getValueFor( IWspmTuhhConstants.POINT_PROPERTY_BREITE ) )
// + "]" );
// }

    final IProfilPointMarkerProvider[] providers = KalypsoModelWspmCoreExtensions.getMarkerProviders( p.getType() );
    if( providers.length != 1 )
      throw new IllegalStateException();

    final IProfilPointMarkerProvider provider = providers[0];

    if( p1 != null )
    {
      final IProfilPointMarker marker = provider.createProfilPointMarker( IWspmTuhhConstants.MARKER_TYP_DURCHSTROEMTE, p1 );
      marker.setValue( true );
    }

    if( p2 != null )
    {
      final IProfilPointMarker marker = provider.createProfilPointMarker( IWspmTuhhConstants.MARKER_TYP_DURCHSTROEMTE, p2 );
      marker.setValue( true );
    }
  }

  private void readWehrtrenner( final double[] values, final IProfil p, final PrfReader pr )
  {
    final IDataBlock dbt = pr.getDataBlock( "TRENNLINIE" );
    if( dbt == null )
      return;
    final double[] pos = dbt.getX();
    for( int i = 0; i < pos.length; i++ )
    {
      final IRecord point = ProfilUtil.findPoint( p, pos[i], 0 );
      if( point != null )
      {
        if( (values != null) && (values.length > i + 1) )
        {
          final IProfilPointPropertyProvider[] providers = KalypsoModelWspmCoreExtensions.getPointPropertyProviders( p.getType() );
          final IComponent cWehr = ProfilObsHelper.getPropertyFromId( providers, IWspmTuhhConstants.MARKER_TYP_WEHR );

          final ProfilDevider devider = new ProfilDevider( cWehr, point );
          devider.setValue( values[i + 1] );
        }
      }
    }
  }

  private boolean readWehr( final IProfil p, final PrfReader pr )
  {
    final IDataBlock dbw = pr.getDataBlock( "OK-WEHR" );
    if( dbw == null )
      return false;

    final IProfileObject wehr = new BuildingWehr( p );
    final String secLine = dbw.getSecondLine();
    final String wehrart = getWehrart( secLine );
    final double[] wt = getWehrParameter( secLine );
    if( wehrart != null )
    {
      final IComponent cWehrart = ProfilObsHelper.getPropertyFromId( wehr, IWspmTuhhConstants.BUILDING_PROPERTY_WEHRART );
      wehr.setValue( cWehrart, wehrart );
    }
    final IComponent cFormbeiwert = ProfilObsHelper.getPropertyFromId( wehr, IWspmTuhhConstants.BUILDING_PROPERTY_FORMBEIWERT );

    wehr.setValue( cFormbeiwert, wt == null ? 0.0 : wt[0] );
    p.addProfileObjects( new IProfileObject[] { wehr } );
    readWehrtrenner( wt, p, pr );
    final IComponent pp = ProfilObsHelper.getPropertyFromId( p, IWspmTuhhConstants.POINT_PROPERTY_OBERKANTEWEHR );
    final double delta = pp == null ? 0.0001 : ProfilObsHelper.getPrecision( pp );
    final PolyLine polyLineO = new PolyLine( dbw.getX(), dbw.getY(), delta );
    final Range rangeO = new Range( polyLineO.getFirstX(), polyLineO.getLastX(), delta );
    for( final IRecord point : p.getPoints() )
    {
      final double breite = (Double) point.getValue( ProfilObsHelper.getPropertyFromId( point, IWspmTuhhConstants.POINT_PROPERTY_BREITE ) );
      final double hoehe = (Double) point.getValue( ProfilObsHelper.getPropertyFromId( point, IWspmTuhhConstants.POINT_PROPERTY_HOEHE ) );

      if( rangeO.contains( breite ) )
        point.setValue( ProfilObsHelper.getPropertyFromId( point, IWspmTuhhConstants.POINT_PROPERTY_OBERKANTEWEHR ), polyLineO.getYFor( breite ) );
      else
        point.setValue( ProfilObsHelper.getPropertyFromId( point, IWspmTuhhConstants.POINT_PROPERTY_OBERKANTEWEHR ), hoehe );

    }
    return true;
  }

  private double[] getWehrParameter( final String params )
  {
    final StringTokenizer sT = new StringTokenizer( params, " " );
    final int paramCount = sT.countTokens() - 1;
    if( paramCount < 1 )
      return null;
    // skip first parameter
    sT.nextToken();

    final double[] wp = new double[paramCount];
    for( int i = 0; i < paramCount; i++ )
    {
      wp[i] = Double.parseDouble( sT.nextToken() );
    }
    return wp;
  }

  private final String getWehrart( final String secLine )
  {
    final StringTokenizer sT = new StringTokenizer( secLine, " " );
    final int paramCount = sT.countTokens() - 1;
    if( paramCount < 0 )
      return null;
    final String wehrart = sT.nextToken().toUpperCase();

    if( wehrart.startsWith( "RUND" ) )
      return IWspmTuhhConstants.WEHR_TYP_RUNDKRONIG;
    if( wehrart.startsWith( "BREI" ) )
      return IWspmTuhhConstants.WEHR_TYP_BREITKRONIG;
    if( wehrart.startsWith( "SCHA" ) )
      return IWspmTuhhConstants.WEHR_TYP_SCHARFKANTIG;
    if( wehrart.startsWith( "BEIW" ) )
      return IWspmTuhhConstants.WEHR_TYP_BEIWERT;

    return null;
  }

  private void readBordVoll( final IProfil p, final PrfReader pr )
  {
    final LinkedList<IRecord> points = p.getPoints();
    if( points.isEmpty() )
      return;
    final IDataBlock db = pr.getDataBlock( "BORDVOLL" );
    if( db == null )
      return;
    final int pCount = db.getCoordCount();
    IRecord p1 = null;
    IRecord p2 = null;

    if( pCount > 0 )
    {
      p1 = ProfilUtil.findPoint( p, db.getX()[0], 0 );
    }
    if( pCount > 1 )
    {
      p2 = ProfilUtil.findPoint( p, db.getX()[1], 0 );
    }
    if( pCount > 2 )
      KalypsoCommonsPlugin.getDefault().getLog().log( new Status( IStatus.INFO, "", 0, "mehr als 2 Datens�tze f�r Bordvollpunkte k�nnen an Station(" + p.getStation() + ") nicht ausgewertet werden", null ) );

    // TODO: KIM in den Reparator verschieben
    // ---------------
// if( p1 == null )
// {
// p1 = points.getFirst();
// m_logger.log( Level.INFO, "Erzeuge Bordvollpunkt f�r Station(" + p.getStation() + ") an Position [" +
// Double.toString( p1.getValueFor( IWspmTuhhConstants.POINT_PROPERTY_BREITE ) ) + "]" );
// }
// if( p2 == null )
// {
// p2 = points.getLast();
// m_logger.log( Level.INFO, "Erzeuge Bordvollpunkt f�r Station(" + p.getStation() + ") an Position [" +
// Double.toString( p2.getValueFor( IWspmTuhhConstants.POINT_PROPERTY_BREITE ) ) + "]" );
// }
    // ----------------------------

    final IProfilPointMarkerProvider[] providers = KalypsoModelWspmCoreExtensions.getMarkerProviders( p.getType() );
    if( providers.length != 1 )
      throw new IllegalStateException();

    final IProfilPointMarkerProvider provider = providers[0];

    if( p1 != null )
    {
      final IProfilPointMarker marker = provider.createProfilPointMarker( IWspmTuhhConstants.MARKER_TYP_BORDVOLL, p1 );
      marker.setValue( true );
    }

    if( p2 != null )
    {
      final IProfilPointMarker marker = provider.createProfilPointMarker( IWspmTuhhConstants.MARKER_TYP_BORDVOLL, p2 );
      marker.setValue( true );
    }
  }

  /**
   * @see org.kalypso.model.wspm.core.profil.serializer.IProfilSource#read(org.kalypso.model.wspm.core.profil.IProfil)
   */
  public boolean read( final IProfil profil, final Reader reader )
  {
    final PrfReader prfReader = new PrfReader();
    try
    {
      prfReader.readFromReader( new BufferedReader( reader ) );
      readSource( prfReader, profil );

      return true;
    }
    catch( final IOException e )
    {
      // TODO: handle exception
      return false;
    }
  }
}