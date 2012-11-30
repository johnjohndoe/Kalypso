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
import java.util.StringTokenizer;

import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.kalypso.commons.KalypsoCommonsPlugin;
import org.kalypso.contribs.java.lang.NumberUtils;
import org.kalypso.model.wspm.core.IWspmConstants;
import org.kalypso.model.wspm.core.KalypsoModelWspmCoreExtensions;
import org.kalypso.model.wspm.core.profil.IProfile;
import org.kalypso.model.wspm.core.profil.IProfileMetadata;
import org.kalypso.model.wspm.core.profil.IProfileObject;
import org.kalypso.model.wspm.core.profil.IProfilePointMarker;
import org.kalypso.model.wspm.core.profil.IProfilePointPropertyProvider;
import org.kalypso.model.wspm.core.profil.ProfileFactory;
import org.kalypso.model.wspm.core.profil.serializer.IProfileSource;
import org.kalypso.model.wspm.core.profil.util.ProfileUtil;
import org.kalypso.model.wspm.core.profil.wrappers.IProfileRecord;
import org.kalypso.model.wspm.core.profil.wrappers.Profiles;
import org.kalypso.model.wspm.tuhh.core.IWspmTuhhConstants;
import org.kalypso.model.wspm.tuhh.core.i18n.Messages;
import org.kalypso.model.wspm.tuhh.core.profile.ProfilDevider;
import org.kalypso.model.wspm.tuhh.core.profile.profileobjects.building.BuildingBruecke;
import org.kalypso.model.wspm.tuhh.core.profile.profileobjects.building.BuildingEi;
import org.kalypso.model.wspm.tuhh.core.profile.profileobjects.building.BuildingKreis;
import org.kalypso.model.wspm.tuhh.core.profile.profileobjects.building.BuildingMaul;
import org.kalypso.model.wspm.tuhh.core.profile.profileobjects.building.BuildingTrapez;
import org.kalypso.model.wspm.tuhh.core.profile.profileobjects.building.BuildingWehr;
import org.kalypso.model.wspm.tuhh.core.profile.profileobjects.building.BuildingWehr.WeirType;
import org.kalypso.model.wspm.tuhh.core.profile.profileobjects.building.IProfileBuilding;
import org.kalypso.model.wspm.tuhh.core.profile.sinuositaet.SINUOSITAET_GERINNE_ART;
import org.kalypso.model.wspm.tuhh.core.profile.sinuositaet.SINUOSITAET_KENNUNG;
import org.kalypso.model.wspm.tuhh.core.profile.sinuositaet.SinuositaetProfileObject;
import org.kalypso.observation.result.IComponent;
import org.kalypso.observation.result.IRecord;
import org.kalypso.ogc.sensor.timeseries.TimeseriesUtils;
import org.kalypso.wspwin.core.prf.IWspWinConstants;
import org.kalypso.wspwin.core.prf.PrfReader;
import org.kalypso.wspwin.core.prf.datablock.DataBlockHeader;
import org.kalypso.wspwin.core.prf.datablock.IDataBlock;
import org.kalypsodeegree.KalypsoDeegreePlugin;

/**
 * @author kimwerner
 */
public class PrfSource implements IProfileSource
{
  private void readSource( final PrfReader pr, final IProfile p )
  {
    if( p == null )
      return;
    try
    {
      final IProfileMetadata metadata = p.getMetadata();
      // p.setProperty( "prfFileFormat_MetaData", pr.getMetaData() ); //$NON-NLS-1$
      metadata.setMetadata( IWspmTuhhConstants.PROFIL_PROPERTY_STATUS, pr.getKeyValue( 3 )[1] );
      metadata.setMetadata( IWspmTuhhConstants.PROFIL_PROPERTY_VERZWEIGUNGSKENNUNG, pr.getKeyValue( 5 )[1] );
      metadata.setMetadata( IWspmTuhhConstants.PROFIL_PROPERTY_WASSERSPIEGEL, pr.getKeyValue( 6 )[1] );
      metadata.setMetadata( IWspmTuhhConstants.PROFIL_PROPERTY_MEHRFELDBRUECKE, pr.getKeyValue( 7 )[1] );
      final String stat = pr.getKeyValue( 9 )[0];
      if( stat.startsWith( "STATION " ) ) //$NON-NLS-1$
      {
        p.setStation( new Double( stat.substring( 10 ) ) );
      }
      if( readPoints( p, pr ) > 0 )
      {
        readTrennFl( p, pr );
        readDurchStr( p, pr );
        readBordVoll( p, pr );
        readRauhheit( p, pr );
        readBewuchs( p, pr );
        readComment( p, pr );
        readGeoCoord( p, pr );
        readSinuositaet( p, pr );
        if( !(readWehr( p, pr ) || readBridge( p, pr )) )
        {
          readBuilding( p, pr );
        }
      }

      final String srsName = findCoordinateSystem( p );
      p.setSrsName( srsName );
    }
    catch( final IllegalArgumentException e )
    {
      KalypsoCommonsPlugin.getDefault().getLog().log( new Status( IStatus.ERROR, KalypsoCommonsPlugin.getID(), 0, e.getMessage(), e ) );
    }
  }

  // FIXME: we probably should let the user set the coordinate system
  // For the moment, we guess the coordinate system by assuming it is gauss-krueger
  private String findCoordinateSystem( final IProfile profile )
  {
    final int rwIndex = profile.indexOfProperty( IWspmConstants.POINT_PROPERTY_RECHTSWERT );
    if( rwIndex == -1 )
      return KalypsoDeegreePlugin.getDefault().getCoordinateSystem();

    final IRecord[] points = profile.getPoints();
    for( final IRecord point : points )
    {
      final Object value = point.getValue( rwIndex );
      if( value instanceof Number )
        return TimeseriesUtils.getCoordinateSystemNameForGkr( value.toString() );
    }

    return null;
  }

  private void readComment( final IProfile p, final PrfReader pr )
  {
    final IDataBlock db = pr.getDataBlock( "KOM" ); //$NON-NLS-1$
    if( db == null )
      return;
    final StringBuffer sb = new StringBuffer();

    final String[] text = db.getText();
    for( int i = 0; i < text.length; i++ )
    {
      final String line = text[i];
      sb.append( line.substring( 3 ) );
      if( i != text.length - 1 )
      {
        sb.append( System.getProperty( "line.separator" ) ); //$NON-NLS-1$
      }
    }
    p.setComment( sb.toString() );
  }

  private void readBewuchs( final IProfile p, final PrfReader pr )
  {
    final IDataBlock dbx = pr.getDataBlock( "AX" ); //$NON-NLS-1$
    final IDataBlock dby = pr.getDataBlock( "AY" ); //$NON-NLS-1$
    final IDataBlock dbp = pr.getDataBlock( "DP" ); //$NON-NLS-1$
    if( dbx == null || dby == null || dbp == null )
      return;

    final IProfilePointPropertyProvider provider = KalypsoModelWspmCoreExtensions.getPointPropertyProviders( p.getType() );

    final IComponent prAx = provider.getPointProperty( IWspmConstants.POINT_PROPERTY_BEWUCHS_AX );
    final IComponent prAy = provider.getPointProperty( IWspmConstants.POINT_PROPERTY_BEWUCHS_AY );
    final IComponent prDp = provider.getPointProperty( IWspmConstants.POINT_PROPERTY_BEWUCHS_DP );

    writePointProperty( p, prAx, dbx );
    writePointProperty( p, prAy, dby );
    writePointProperty( p, prDp, dbp );
  }

  private void writePointProperty( final IProfile p, final IComponent property, final IDataBlock db )
  {
    p.addPointProperty( property );
    final int index = p.indexOfProperty( property );
    if( index < 0 )
    {
      KalypsoCommonsPlugin.getDefault().getLog().log( new Status( IStatus.ERROR, KalypsoCommonsPlugin.getID(), 0, Messages.getString( "org.kalypso.model.wspm.tuhh.core.wspwin.prf.PrfSource.7", property.getName() ), null ) ); //$NON-NLS-1$
      return;
    }

    final Double[] xs = db.getX();
    final Double[] ys = db.getY();
    for( int i = 0; i < xs.length; i++ )
    {
      final IRecord point = ProfileUtil.findPoint( p, i, xs[i], 0 );
      if( point != null )
      {
        point.setValue( index, ys[i] );
      }
    }
  }

  private void readGeoCoord( final IProfile p, final PrfReader pr )
  {
    final IDataBlock dbh = pr.getDataBlock( "HOC" ); //$NON-NLS-1$
    final IDataBlock dbr = pr.getDataBlock( "REC" ); //$NON-NLS-1$
    if( dbh == null || dbr == null )
      return;

    final IProfilePointPropertyProvider provider = KalypsoModelWspmCoreExtensions.getPointPropertyProviders( p.getType() );

    final IComponent hochwert = provider.getPointProperty( IWspmConstants.POINT_PROPERTY_HOCHWERT );
    final IComponent rechtswert = provider.getPointProperty( IWspmConstants.POINT_PROPERTY_RECHTSWERT );

    writePointProperty( p, hochwert, dbh );
    writePointProperty( p, rechtswert, dbr );
  }

  private void readSinuositaet( final IProfile p, final PrfReader pr )
  {
    final IDataBlock db = pr.getDataBlock( IPrfConstants.HEADER_SINUOSITAET ); //$NON-NLS-1$
    if( db == null )
      return;

    final Double[] sin = db.getX();
    final SinuositaetProfileObject profileObject = new SinuositaetProfileObject();

    profileObject.setKennung( SINUOSITAET_KENNUNG.fromInteger( sin[0].intValue() ).name() );
    profileObject.setSn( sin[1] );
    profileObject.setGerinneArt( SINUOSITAET_GERINNE_ART.fromInteger( sin[2].intValue() ).name() );
    profileObject.setLf( sin[3] );

    p.addProfileObjects( profileObject );
  }

  private void readBuilding( final IProfile p, final PrfReader pr )
  {
    IDataBlock db = pr.getDataBlock( "EI" ); //$NON-NLS-1$
    if( db == null )
      db = pr.getDataBlock( "TRA" ); //$NON-NLS-1$

    if( db == null )
      db = pr.getDataBlock( "KRE" ); //$NON-NLS-1$

    if( db == null )
      db = pr.getDataBlock( "MAU" ); //$NON-NLS-1$

    if( db == null )
      return;

    final DataBlockHeader dbh = db.getDataBlockHeader();
    final Double[] values = db.getX();

    double rauheit = 0.0;
    final IDataBlock dbRau = pr.getDataBlock( "RAU" ); //$NON-NLS-1$
    if( dbRau != null && dbRau.getY().length > 0 )
      rauheit = dbRau.getY()[0];

    final IProfileBuilding building = getProfileBuilding( dbh.getSpecification( 8 ), values, rauheit );

    p.addProfileObjects( new IProfileObject[] { building } );
  }

  private IProfileBuilding getProfileBuilding( final Integer specification, final Double[] values, final double rauheit )
  {
    // important: changing property-positions will cause wrong parameter for building
    switch( specification )
    {
      case IWspWinConstants.SPEZIALPROFIL_TRAPEZ:
      {
        final BuildingTrapez building = new BuildingTrapez();
        building.setBreite( values[0] );
        building.setHoehe( values[1] );
        building.setSteigung( values[2] );
        building.setSohlgefaelle( values[3] );
        building.setBezugspunktX( values[4] );
        building.setBezugspunktY( values[5] );
        building.setRauheit( rauheit );

        return building;
      }
      case IWspWinConstants.SPEZIALPROFIL_KREIS:
      {
        final BuildingKreis building = new BuildingKreis();
        building.setBreite( values[0] );
        building.setSohlgefaelle( values[1] );
        building.setBezugspunktX( values[2] );
        building.setBezugspunktY( values[3] );
        building.setRauheit( rauheit );

        return building;
      }
      case IWspWinConstants.SPEZIALPROFIL_EI:
      {
        final BuildingEi building = new BuildingEi();
        building.setBreite( values[0] );
        building.setHoehe( values[1] );
        building.setSohlgefaelle( values[2] );
        building.setBezugspunktX( values[3] );
        building.setBezugspunktY( values[4] );
        building.setRauheit( rauheit );

        return building;
      }
      case IWspWinConstants.SPEZIALPROFIL_MAUL:
      {
        final BuildingMaul building = new BuildingMaul();
        building.setBreite( values[0] );
        building.setHoehe( values[1] );
        building.setSohlgefaelle( values[2] );
        building.setBezugspunktX( values[3] );
        building.setBezugspunktY( values[4] );
        building.setRauheit( rauheit );

        return building;
      }
      default:
        return null;
    }
  }

  private boolean readBridge( final IProfile p, final PrfReader pr )
  {
    final IDataBlock dbo = pr.getDataBlock( "OK-B" ); //$NON-NLS-1$
    final IDataBlock dbu = pr.getDataBlock( "UK-B" ); //$NON-NLS-1$
    if( dbo == null || dbu == null )
      return false;

    final BuildingBruecke bridge = new BuildingBruecke( p );
    final StringTokenizer sT = new StringTokenizer( dbu.getSecondLine(), " " ); //$NON-NLS-1$
    if( sT.countTokens() > 4 )
      KalypsoCommonsPlugin.getDefault().getLog().log( new Status( IStatus.WARNING, KalypsoCommonsPlugin.getID(), 0, Messages.getString( "org.kalypso.model.wspm.tuhh.core.wspwin.prf.PrfSource.20" ), null ) ); //$NON-NLS-1$

    final Double unterwasser = getValue( sT.nextToken() );
    final Double breite = getValue( sT.nextToken() );
    final Double rauheit = getValue( sT.nextToken() );
    final Double formbeiwert = getValue( sT.nextToken() );

    bridge.setUnterwasser( unterwasser );
    bridge.setBreite( breite );
    bridge.setRauheit( rauheit );
    bridge.setFormbeiwert( formbeiwert );

    p.addProfileObjects( new IProfileObject[] { bridge } );

    insertBuildingValues( p, dbu, IWspmTuhhConstants.POINT_PROPERTY_UNTERKANTEBRUECKE );
    insertBuildingValues( p, dbo, IWspmTuhhConstants.POINT_PROPERTY_OBERKANTEBRUECKE );

    return true;
  }

  private Double getValue( final String token )
  {
    final double value = NumberUtils.parseQuietDouble( token );
    if( Double.isNaN( value ) )
      return null;

    return new Double( value );
  }

  private int readPoints( final IProfile p, final PrfReader pr )
  {
    final IDataBlock db = pr.getDataBlock( "Gelae" ); //$NON-NLS-1$
    if( db == null )
      return 0;
    final Double[] xs = db.getX();
    final Double[] ys = db.getY();
    final IComponent cBreite = p.getPointPropertyFor( IWspmConstants.POINT_PROPERTY_BREITE );
    final IComponent cHoehe = p.getPointPropertyFor( IWspmConstants.POINT_PROPERTY_HOEHE );
    p.addPointProperty( cBreite );
    p.addPointProperty( cHoehe );

    for( int i = 0; i < xs.length; i++ )
    {
      final IProfileRecord point = p.createProfilPoint();
      point.setBreite( xs[i] );
      point.setHoehe( ys[i] );
      p.addPoint( point );
    }
    return xs.length;
  }

  private void readRauhheit( final IProfile p, final PrfReader pr )
  {
    final IDataBlock db = pr.getDataBlock( "RAU" ); //$NON-NLS-1$
    if( db == null )
      return;

    final String rks = db.getSecondLine().toUpperCase();

    IComponent rTyp = null;
    if( rks.startsWith( "KST" ) ) //$NON-NLS-1$
    {
      rTyp = p.getPointPropertyFor( IWspmConstants.POINT_PROPERTY_RAUHEIT_KST );
    }
    else if( rks.startsWith( "KS" ) || rks.startsWith( "K-S " ) ) //$NON-NLS-1$ //$NON-NLS-2$
    {
      rTyp = p.getPointPropertyFor( IWspmConstants.POINT_PROPERTY_RAUHEIT_KS );
    }

    if( rTyp == null )
    {
      KalypsoCommonsPlugin.getDefault().getLog().log( new Status( IStatus.INFO, KalypsoCommonsPlugin.getID(), 0, Messages.getString( "org.kalypso.model.wspm.tuhh.core.wspwin.prf.PrfSource.26", rks ), null ) ); //$NON-NLS-1$ //$NON-NLS-2$
      return;
    }
    p.addPointProperty( rTyp );
    final int index = p.indexOfProperty( rTyp );
    for( int i = 0; i < db.getCoordCount(); i++ )
    {
      final IRecord point = ProfileUtil.findPoint( p, i, db.getX()[i], 0 );
      if( point != null )
      {
        point.setValue( index, db.getY()[i] );
      }
    }
  }

  private void readTrennFl( final IProfile p, final PrfReader pr )
  {
    final IRecord[] points = p.getPoints();
    if( points.length == 0 )
      return;
    final IDataBlock db = pr.getDataBlock( "TRENNFLAECHEN" ); //$NON-NLS-1$
    if( db == null )
      return;
    final int pCount = db.getCoordCount();

    IProfileRecord p1 = null;
    IProfileRecord p2 = null;
    int pos1 = 0;
    int pos2 = 0;

    if( pCount > 0 )
    {
      p1 = ProfileUtil.findPoint( p, db.getX()[0], 0 );
      pos1 = (int)db.getY()[0].doubleValue();
    }
    if( pCount > 1 )
    {
      p2 = ProfileUtil.findPoint( p, db.getX()[1], 0 );
      pos2 = (int)db.getY()[1].doubleValue();
    }
    if( pCount > 2 )
    {
      KalypsoCommonsPlugin.getDefault().getLog().log( new Status( IStatus.INFO, KalypsoCommonsPlugin.getID(), 0, Messages.getString( "org.kalypso.model.wspm.tuhh.core.wspwin.prf.PrfSource.29", p.getStation() ) //$NON-NLS-1$
      , null ) ); //$NON-NLS-1$
    }

    if( p1 != null )
    {
      final IProfilePointMarker marker = p.createPointMarker( IWspmTuhhConstants.MARKER_TYP_TRENNFLAECHE, p1 );
      marker.setInterpretedValue( pos1 == 3 );
    }

    if( p2 != null )
    {
      final IProfilePointMarker marker = p.createPointMarker( IWspmTuhhConstants.MARKER_TYP_TRENNFLAECHE, p2 );
      marker.setInterpretedValue( pos2 == 4 );
    }
  }

  private void readDurchStr( final IProfile p, final PrfReader pr )
  {
    final IProfileRecord[] points = p.getPoints();
    if( points.length == 0 )
      return;
    final IDataBlock db = pr.getDataBlock( "DURCHSTROEMTE" ); //$NON-NLS-1$
    if( db == null )
      return;
    final int pCount = db.getCoordCount();

    IProfileRecord p1 = null;
    IProfileRecord p2 = null;

    if( pCount > 0 )
    {
      p1 = ProfileUtil.findPoint( p, db.getX()[0], 0 );
    }
    if( pCount > 1 )
    {
      p2 = ProfileUtil.findPoint( p, db.getX()[1], 0 );
    }
    if( pCount > 2 )
    {
      KalypsoCommonsPlugin.getDefault().getLog().log( new Status( IStatus.INFO, KalypsoCommonsPlugin.getID(), 0, Messages.getString( "org.kalypso.model.wspm.tuhh.core.wspwin.prf.PrfSource.32", p.getStation() ) //$NON-NLS-1$
      , null ) ); //$NON-NLS-1$
    }

    if( p1 != null )
    {
      final IProfilePointMarker marker = p.createPointMarker( IWspmTuhhConstants.MARKER_TYP_DURCHSTROEMTE, p1 );
      marker.setValue( true );
    }

    if( p2 != null )
    {
      final IProfilePointMarker marker = p.createPointMarker( IWspmTuhhConstants.MARKER_TYP_DURCHSTROEMTE, p2 );
      marker.setValue( true );
    }
  }

  private void readWehrtrenner( final double[] values, final IProfile p, final PrfReader pr )
  {
    final IProfilePointPropertyProvider provider = KalypsoModelWspmCoreExtensions.getPointPropertyProviders( p.getType() );

    final IDataBlock dbt = pr.getDataBlock( "TRENNLINIE" ); //$NON-NLS-1$
    if( dbt == null )
      return;

    final Double[] pos = dbt.getX();
    for( int i = 0; i < pos.length; i++ )
    {
      final IProfileRecord point = ProfileUtil.findPoint( p, pos[i], 0 );
      if( point != null )
      {
        if( values != null && values.length > i + 1 )
        {
          final IComponent cWehr = provider.getPointProperty( IWspmTuhhConstants.MARKER_TYP_WEHR );

          final ProfilDevider devider = new ProfilDevider( cWehr, point );
          devider.setValue( values[i + 1] );
        }
      }
    }
  }

  private boolean readWehr( final IProfile profile, final PrfReader pr )
  {
    final IDataBlock dbw = pr.getDataBlock( "OK-WEHR" ); //$NON-NLS-1$
    if( dbw == null )
      return false;

    final BuildingWehr wehr = new BuildingWehr( profile );
    final String secLine = dbw.getSecondLine();

    final WeirType wehrart = getWehrart( secLine );
    wehr.setWehrart( wehrart );

    final double[] wt = getWehrParameter( secLine );

    wehr.setFormbeiwert( wt == null ? 0.0 : wt[0] );

    profile.addProfileObjects( new IProfileObject[] { wehr } );

    readWehrtrenner( wt, profile, pr );

    insertBuildingValues( profile, dbw, IWspmTuhhConstants.POINT_PROPERTY_OBERKANTEWEHR );

    return true;
  }

  private double[] getWehrParameter( final String params )
  {
    final StringTokenizer sT = new StringTokenizer( params, " " ); //$NON-NLS-1$
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

  private final WeirType getWehrart( final String secLine )
  {
    final StringTokenizer sT = new StringTokenizer( secLine, " " ); //$NON-NLS-1$
    final int paramCount = sT.countTokens() - 1;
    if( paramCount < 0 )
      return null;

    final String wehrart = sT.nextToken().toUpperCase();

    if( wehrart.startsWith( "RUND" ) ) //$NON-NLS-1$
      return WeirType.rundkronig;

    if( wehrart.startsWith( "BREI" ) ) //$NON-NLS-1$
      return WeirType.breitkronig;

    if( wehrart.startsWith( "SCHA" ) ) //$NON-NLS-1$
      return WeirType.scharfkantig;

    if( wehrart.startsWith( "BEIW" ) ) //$NON-NLS-1$
      return WeirType.beiwert;

    return BuildingWehr.DEFAULT_WEIRTYPE;
  }

  private void readBordVoll( final IProfile p, final PrfReader pr )
  {
    final IProfileRecord[] points = p.getPoints();
    if( points.length == 0 )
      return;
    final IDataBlock db = pr.getDataBlock( "BORDVOLL" ); //$NON-NLS-1$
    if( db == null )
      return;
    final int pCount = db.getCoordCount();
    IProfileRecord p1 = null;
    IProfileRecord p2 = null;

    if( pCount > 0 )
    {
      p1 = ProfileUtil.findPoint( p, db.getX()[0], 0 );
    }
    if( pCount > 1 )
    {
      p2 = ProfileUtil.findPoint( p, db.getX()[1], 0 );
    }
    if( pCount > 2 )
    {
      KalypsoCommonsPlugin.getDefault().getLog().log( new Status( IStatus.INFO, KalypsoCommonsPlugin.getID(), 0, Messages.getString( "org.kalypso.model.wspm.tuhh.core.wspwin.prf.PrfSource.43", p.getStation() ) //$NON-NLS-1$
      , null ) ); //$NON-NLS-1$
    }

    if( p1 != null )
    {
      final IProfilePointMarker marker = p.createPointMarker( IWspmTuhhConstants.MARKER_TYP_BORDVOLL, p1 );
      marker.setValue( true );
    }

    if( p2 != null )
    {
      final IProfilePointMarker marker = p.createPointMarker( IWspmTuhhConstants.MARKER_TYP_BORDVOLL, p2 );
      marker.setValue( true );
    }
  }

  @Override
  public IProfile[] read( final String profileTyp, final Reader reader ) throws IOException
  {
    final IProfile profil = ProfileFactory.createProfil( profileTyp, null );

    if( profil == null )
      throw new IOException( Messages.getString( "PrfSource.0" ) + profileTyp ); //$NON-NLS-1$

    final PrfReader prfReader = new PrfReader();
    prfReader.readFromReader( new BufferedReader( reader ) );
    readSource( prfReader, profil );
    return new IProfile[] { profil };
  }

  private void insertBuildingValues( final IProfile profile, final IDataBlock dbw, final String componentID )
  {
    final int componentIndex = ProfileUtil.getOrCreateComponent( profile, componentID );
    final IComponent component = profile.getPointProperties()[componentIndex];

    final double precision = component.getPrecision();

    final Double[] xValues = dbw.getX();
    final Double[] yValues = dbw.getY();

    if( xValues.length != yValues.length )
      return;

    for( int i = 0; i < yValues.length; i++ )
    {
      final Double x = xValues[i];
      final Double y = yValues[i];

      final IRecord weirRecord = findOrInsertPointAt( profile, x, componentIndex, precision );
      weirRecord.setValue( componentIndex, y );
    }
  }

  private IRecord findOrInsertPointAt( final IProfile profile, final double distance, final int buildPropertyIndex, final double precision )
  {
    final IRecord existingPoint = ProfileUtil.findPoint( profile, distance, precision );
    if( existingPoint != null )
    {
      // REAMRK: only if the building has no value, return the existing point
      final Object buildingValue = existingPoint.getValue( buildPropertyIndex );
      if( buildingValue == null )
        return existingPoint;
    }

    // If no point with this width exist or if it already has the buildingProperty, create a new one:
    return Profiles.addOrFindPoint( profile, distance );
  }

}