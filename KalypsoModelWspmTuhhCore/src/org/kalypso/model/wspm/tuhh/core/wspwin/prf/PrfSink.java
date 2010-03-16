/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 * 
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestraï¿½e 22
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

import java.io.IOException;
import java.io.LineNumberReader;
import java.io.PrintWriter;
import java.io.StringReader;
import java.io.Writer;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;

import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.kalypso.commons.KalypsoCommonsPlugin;
import org.kalypso.model.wspm.core.IWspmConstants;
import org.kalypso.model.wspm.core.profil.IProfil;
import org.kalypso.model.wspm.core.profil.IProfilPointMarker;
import org.kalypso.model.wspm.core.profil.IProfileObject;
import org.kalypso.model.wspm.core.profil.serializer.IProfilSink;
import org.kalypso.model.wspm.core.profil.util.ProfilUtil;
import org.kalypso.model.wspm.tuhh.core.IWspmTuhhConstants;
import org.kalypso.model.wspm.tuhh.core.i18n.Messages;
import org.kalypso.observation.result.IComponent;
import org.kalypso.observation.result.IRecord;
import org.kalypso.wspwin.core.prf.DataBlockWriter;
import org.kalypso.wspwin.core.prf.datablock.CoordDataBlock;
import org.kalypso.wspwin.core.prf.datablock.DataBlockHeader;
import org.kalypso.wspwin.core.prf.datablock.IDataBlock;
import org.kalypso.wspwin.core.prf.datablock.TextDataBlock;

/**
 * @author kimwerner
 */
public class PrfSink implements IProfilSink
{
  private String toDataBlockKey( final Object profilKey )
  {
    if( IWspmTuhhConstants.WEHR_TYP_BEIWERT.equals( profilKey ) )
      return "BEIWERT"; //$NON-NLS-1$
    else if( IWspmTuhhConstants.WEHR_TYP_RUNDKRONIG.equals( profilKey ) )
      return "RUNDKRONIG"; //$NON-NLS-1$
    else if( IWspmTuhhConstants.WEHR_TYP_SCHARFKANTIG.equals( profilKey ) )
      return "SCHARFKANTIG"; //$NON-NLS-1$
    else if( IWspmTuhhConstants.WEHR_TYP_BREITKRONIG.equals( profilKey ) )
      return "BREITKRONIG"; //$NON-NLS-1$
    else
      return profilKey.toString();
  }

  private void extractDataBlocks( final DataBlockWriter pw, final IProfil p )
  {
    writePoints( pw, p );
    writeDevider( pw, p );
    writeRauheit( pw, p );

    // FIXME: spezial Zeugs für Steiermark, aber wohin?
    writeWaterlevel( pw, p );

    if( p.getProfileObjects() != null )
      writeBuilding( pw, p );
    if( p.hasPointProperty( IWspmConstants.POINT_PROPERTY_HOCHWERT ) != null )
      writeHochRechts( pw, p );
    if( p.hasPointProperty( IWspmConstants.POINT_PROPERTY_BEWUCHS_AX ) != null )
      writeBewuchs( pw, p );
    if( p.getComment() != null )
      writeComment( pw, p );
  }

  /**
   * Experimental: only used for steiermark project....
   */
  private void writeWaterlevel( final DataBlockWriter pw, final IProfil p )
  {
    final PlotterWaterlevelWriter plotterExporter = new PlotterWaterlevelWriter( p );
    final IDataBlock[] dbs = plotterExporter.createDataBlocks();
    for( final IDataBlock dataBlock : dbs )
      pw.addDataBlock( dataBlock );
  }

  private void writeComment( final DataBlockWriter pw, final IProfil profil )
  {
    final String comment = profil.getComment();
    final DataBlockHeader dbh = createHeader( "KOM" ); //$NON-NLS-1$
    final TextDataBlock db = new TextDataBlock( dbh );

    final StringReader stringReader = new StringReader( comment );
    final LineNumberReader lineNumberReader = new LineNumberReader( stringReader );

    try
    {
      for( String line = lineNumberReader.readLine(); line != null; line = lineNumberReader.readLine() )
        db.addLine( "CC " + line ); //$NON-NLS-1$
      if( db.getCoordCount() > 0 )
      {
        db.setThirdLine( "0  0  0  0  0  0  0  " + Integer.toString( db.getCoordCount() ) + " 17" ); //$NON-NLS-1$ //$NON-NLS-2$
        pw.addDataBlock( db );
      }
    }
    catch( final IOException e )
    {
      KalypsoCommonsPlugin.getDefault().getLog().log( new Status( IStatus.WARNING, KalypsoCommonsPlugin.getID(), 0, Messages.getString( "org.kalypso.model.wspm.tuhh.core.wspwin.prf.PrfSink.8" ), e ) ); //$NON-NLS-1$
    }
  }

  private void writePoints( final DataBlockWriter pw, final IProfil profil )
  {
    final DataBlockHeader dbh = createHeader( "GEL" ); //$NON-NLS-1$
    final CoordDataBlock db = new CoordDataBlock( dbh );
    writeCoords( profil, profil.hasPointProperty( IWspmConstants.POINT_PROPERTY_HOEHE ), db );
    pw.addDataBlock( db );
  }

  private final boolean istDurchlass( final IProfileObject[] objects )
  {
    if( objects == null || objects.length < 1 )
      return false;
    final String building = objects[0].getId();
    if( building.equals( IWspmTuhhConstants.BUILDING_TYP_EI ) || building.equals( IWspmTuhhConstants.BUILDING_TYP_MAUL ) || building.equals( IWspmTuhhConstants.BUILDING_TYP_KREIS )
        || building.equals( IWspmTuhhConstants.BUILDING_TYP_TRAPEZ ) )
      return true;
    return false;

  }

  private void writeRauheit( final DataBlockWriter pw, final IProfil profil )
  {
    CoordDataBlock dbr = null;
    if( profil.hasPointProperty( IWspmConstants.POINT_PROPERTY_RAUHEIT_KS ) != null )
    {
      final DataBlockHeader dbhr = createHeader( "KS" ); //$NON-NLS-1$
      dbr = new CoordDataBlock( dbhr );
      writeCoords( profil, profil.hasPointProperty( IWspmConstants.POINT_PROPERTY_RAUHEIT_KS ), dbr );
    }
    else if( profil.hasPointProperty( IWspmConstants.POINT_PROPERTY_RAUHEIT_KST ) != null )
    {
      final DataBlockHeader dbhr = createHeader( "KST" ); //$NON-NLS-1$
      dbr = new CoordDataBlock( dbhr );
      writeCoords( profil, profil.hasPointProperty( IWspmConstants.POINT_PROPERTY_RAUHEIT_KST ), dbr );
    }
    else
    {
      // TODO: if (isDurchlass(buildings) {final DataBlockHeader dbhr = createHeader( building.getRauheitTyp()
      // );}

      final DataBlockHeader dbhr = createHeader( "KS" ); //$NON-NLS-1$
      dbr = new CoordDataBlock( dbhr );
      final int size = profil.getPoints().length;
      dbr.setCoords( new Double[size], new Double[size] );
    }

    final IProfileObject[] buildings = profil.getProfileObjects();
    if( istDurchlass( buildings ) )
    {
      final Double roughness = ProfilUtil.getDoubleValueFor( IWspmTuhhConstants.BUILDING_PROPERTY_RAUHEIT, buildings[0] );
      for( int i = 0; i < dbr.getY().length; i++ )
      {
        dbr.getY()[i] = roughness;
      }
    }
    pw.addDataBlock( dbr );
  }

  private final DataBlockHeader createHeader( final String key )
  {
    final DataBlockHeader dbh = new DataBlockHeader();

    if( key.startsWith( "GEL" ) ) //$NON-NLS-1$
    {
      dbh.setFirstLine( "GELAENDE-" ); //$NON-NLS-1$
      dbh.setSecondLine( "HOEHE" ); //$NON-NLS-1$
    }
    else if( key.startsWith( "TRENNF" ) ) //$NON-NLS-1$
    {
      dbh.setFirstLine( "TRENNFLAECHEN" ); //$NON-NLS-1$
    }
    else if( key.startsWith( "DUR" ) ) //$NON-NLS-1$
    {
      dbh.setFirstLine( "DURCHSTROEMTE" ); //$NON-NLS-1$
      dbh.setSecondLine( "BEREICHE" ); //$NON-NLS-1$
    }
    else if( key.startsWith( "KST" ) ) //$NON-NLS-1$
    {
      dbh.setFirstLine( "RAUHEIT" ); //$NON-NLS-1$
      dbh.setSecondLine( "kst   m" ); //$NON-NLS-1$
    }
    else if( key.startsWith( "KS" ) ) //$NON-NLS-1$
    {
      dbh.setFirstLine( "RAUHEIT" ); //$NON-NLS-1$
      dbh.setSecondLine( "k-s   m" ); //$NON-NLS-1$
    }
    else if( key.startsWith( "REC" ) ) //$NON-NLS-1$
    {
      dbh.setFirstLine( "RECHTSWERT" ); //$NON-NLS-1$
    }
    else if( key.startsWith( "HOC" ) ) //$NON-NLS-1$
    {
      dbh.setFirstLine( "HOCHWERT" ); //$NON-NLS-1$
    }
    else if( key.startsWith( "UK-B" ) ) //$NON-NLS-1$
    {
      dbh.setFirstLine( "UK-BRUECKE" ); //$NON-NLS-1$
    }
    else if( key.startsWith( "OK-B" ) ) //$NON-NLS-1$
    {
      dbh.setFirstLine( "OK-BRUECKE" ); //$NON-NLS-1$
    }
    else if( key.startsWith( "KOM" ) ) //$NON-NLS-1$
    {
      dbh.setFirstLine( "KOMMENTAR:" ); //$NON-NLS-1$
    }
    else if( key.startsWith( "BOR" ) ) //$NON-NLS-1$
    {
      dbh.setFirstLine( "BORDVOLL" ); //$NON-NLS-1$
    }
    else if( key.startsWith( "AX" ) ) //$NON-NLS-1$
    {
      dbh.setFirstLine( "AX   m" ); //$NON-NLS-1$
    }
    else if( key.startsWith( "AY" ) ) //$NON-NLS-1$
    {
      dbh.setFirstLine( "AY   m" ); //$NON-NLS-1$
    }
    else if( key.startsWith( "DP" ) ) //$NON-NLS-1$
    {
      dbh.setFirstLine( "DP   m" ); //$NON-NLS-1$
    }
    else if( key.startsWith( "EI" ) ) //$NON-NLS-1$
    {
      dbh.setFirstLine( "EI" ); //$NON-NLS-1$
    }
    else if( key.startsWith( "KRE" ) ) //$NON-NLS-1$
    {
      dbh.setFirstLine( "KREIS" ); //$NON-NLS-1$
    }
    else if( key.startsWith( "TRA" ) ) //$NON-NLS-1$
    {
      dbh.setFirstLine( "TRAPEZ" ); //$NON-NLS-1$
    }
    else if( key.startsWith( "MAU" ) ) //$NON-NLS-1$
    {
      dbh.setFirstLine( "MAULPROFIL" ); //$NON-NLS-1$
    }
    else if( key.startsWith( "OK-W" ) ) //$NON-NLS-1$
    {
      dbh.setFirstLine( "OK-WEHR" ); //$NON-NLS-1$
    }
    else if( key.startsWith( "TRENNL" ) ) //$NON-NLS-1$
    {
      dbh.setFirstLine( "TRENNLINIE" ); //$NON-NLS-1$
      dbh.setSecondLine( "WEHR" ); //$NON-NLS-1$
    }
    else
    {
      dbh.setFirstLine( key );
    }
    return dbh;
  }

  private void writeCoords( final IProfil profil, final IComponent prop, final CoordDataBlock db )
  {
    final IRecord[] points = profil.getPoints();

    final List<Double> xs = new ArrayList<Double>( points.length );
    final List<Double> ys = new ArrayList<Double>( points.length );

    final int iBreite = profil.indexOfProperty( IWspmConstants.POINT_PROPERTY_BREITE );
    final int iProp = profil.indexOfProperty( prop );
    for( int i = 0; i < points.length; i++ )
    {
      try
      {
        final Object vBreite = points[i].getValue( iBreite );
        final Object vProp = points[i].getValue( iProp );

        // FIXME: das ist neu: vorher wurden Punkte mit vProp == null als 0.0 geschrieben.
        if( vBreite instanceof Number && vProp instanceof Number )
        {
          xs.add( ((Number) vBreite).doubleValue() );
          ys.add( ((Number) vProp).doubleValue() );
        }
        else if( vBreite instanceof Number )
        {
          xs.add( ((Number) vBreite).doubleValue() );
          ys.add( 0.0 );
        }
      }
      catch( final Exception e )
      {
        KalypsoCommonsPlugin.getDefault().getLog().log( new Status( IStatus.ERROR, KalypsoCommonsPlugin.getID(), 0, Messages.getString( "org.kalypso.model.wspm.tuhh.core.wspwin.prf.PrfSink.13", prop.getName(), Integer.toString( i ) ), e ) ); //$NON-NLS-1$
      }
    }

    final Double[] xArray = xs.toArray( new Double[xs.size()] );
    final Double[] yArray = ys.toArray( new Double[ys.size()] );
    db.setCoords( xArray, yArray );
  }

  private void writeHochRechts( final DataBlockWriter pw, final IProfil profil )
  {
    final DataBlockHeader dbhh = createHeader( "HOC" ); //$NON-NLS-1$
    final CoordDataBlock dbh = new CoordDataBlock( dbhh );
    writeCoords( profil, profil.hasPointProperty( IWspmConstants.POINT_PROPERTY_HOCHWERT ), dbh );
    pw.addDataBlock( dbh );

    final DataBlockHeader dbhr = createHeader( "REC" ); //$NON-NLS-1$
    final CoordDataBlock dbr = new CoordDataBlock( dbhr );
    writeCoords( profil, profil.hasPointProperty( IWspmConstants.POINT_PROPERTY_RECHTSWERT ), dbr );
    pw.addDataBlock( dbr );
  }

  private void writeDevider( final DataBlockWriter pw, final IProfil profil )
  {
    writeDeviderTyp( pw, "TRENNF", profil.getPointMarkerFor( profil.hasPointProperty( IWspmTuhhConstants.MARKER_TYP_TRENNFLAECHE ) ), profil ); //$NON-NLS-1$
    writeDeviderTyp( pw, "BOR", profil.getPointMarkerFor( profil.hasPointProperty( IWspmTuhhConstants.MARKER_TYP_BORDVOLL ) ), profil ); //$NON-NLS-1$
    writeDeviderTyp( pw, "DUR", profil.getPointMarkerFor( profil.hasPointProperty( IWspmTuhhConstants.MARKER_TYP_DURCHSTROEMTE ) ), profil ); //$NON-NLS-1$
    writeDeviderTyp( pw, "TRENNL", profil.getPointMarkerFor( profil.hasPointProperty( IWspmTuhhConstants.MARKER_TYP_WEHR ) ), profil ); //$NON-NLS-1$
  }

  private void writeDeviderTyp( final DataBlockWriter pw, final String key, final IProfilPointMarker[] deviders, final IProfil profil )
  {
    if( deviders == null || deviders.length == 0 )
      return;

    final CoordDataBlock dbw = new CoordDataBlock( createHeader( key ) );
    final Double[] xs = new Double[deviders.length];
    final Double[] ys = new Double[deviders.length];
    final int iBreite = profil.indexOfProperty( IWspmConstants.POINT_PROPERTY_BREITE );
    for( int i = 0; i < deviders.length; i++ )
    {
      final IProfilPointMarker devider = deviders[i];
      final IRecord point = devider.getPoint();

      try
      {
        xs[i] = (Double) point.getValue( iBreite );
        ys[i] = getDeviderYValue( devider, i, profil );
      }
      catch( final Exception e )
      {
        KalypsoCommonsPlugin.getDefault().getLog().log( new Status( IStatus.ERROR, KalypsoCommonsPlugin.getID(), 0, Messages.getString( "org.kalypso.model.wspm.tuhh.core.wspwin.prf.PrfSink.21", devider.getId().toString() ), e ) ); //$NON-NLS-1$
      }
    }
    dbw.setCoords( xs, ys );
    pw.addDataBlock( dbw );
  }

  /**
   * Get coodinate y value: depends on type of marker.
   */
  private double getDeviderYValue( final IProfilPointMarker devider, final int index, final IProfil profil )
  {
    final IComponent markerId = devider.getId();

    if( IWspmTuhhConstants.MARKER_TYP_TRENNFLAECHE.equals( markerId.getId() ) )
    {
      final boolean isBoeschung = devider.getIntepretedValue() == null ? false : (Boolean) devider.getIntepretedValue();
      final int offset = isBoeschung ? 3 : 1;
      return offset + index;
    }

    if( IWspmTuhhConstants.MARKER_TYP_BORDVOLL.equals( markerId.getId() ) )
      return index + 1;
    final int iHoehe = profil.indexOfProperty( IWspmConstants.POINT_PROPERTY_HOEHE );
    return (Double) devider.getPoint().getValue( iHoehe );
  }

  private void writeBuilding( final DataBlockWriter pw, final IProfil profil )
  {
    final IProfileObject[] buildings = profil.getProfileObjects();

    IProfileObject building = null;
    if( buildings.length > 0 )
      building = buildings[0];

    final String buildingTyp = building == null ? "" : building.getId(); //$NON-NLS-1$
    if( buildingTyp.equals( IWspmTuhhConstants.BUILDING_TYP_BRUECKE ) )
    {
      final DataBlockHeader dbho = createHeader( "OK-B" ); //$NON-NLS-1$
      final CoordDataBlock dbo = new CoordDataBlock( dbho );
      writeCoords( profil, profil.hasPointProperty( IWspmTuhhConstants.POINT_PROPERTY_OBERKANTEBRUECKE ), dbo );
      pw.addDataBlock( dbo );
      final DataBlockHeader dbhu = createHeader( "UK-B" ); //$NON-NLS-1$
      final CoordDataBlock dbu = new CoordDataBlock( dbhu );
      writeCoords( profil, profil.hasPointProperty( IWspmTuhhConstants.POINT_PROPERTY_UNTERKANTEBRUECKE ), dbu );
      try
      {
        final String secLine = String.format( Locale.US, " %12.4f", building.getValueFor( IWspmTuhhConstants.BUILDING_PROPERTY_UNTERWASSER ) ) //$NON-NLS-1$
            + String.format( Locale.US, " %12.4f", building.getValueFor( IWspmTuhhConstants.BUILDING_PROPERTY_BREITE ) ) //$NON-NLS-1$
            + String.format( Locale.US, " %12.4f", building.getValueFor( IWspmTuhhConstants.BUILDING_PROPERTY_RAUHEIT ) ) //$NON-NLS-1$
            + String.format( Locale.US, " %12.4f", building.getValueFor( IWspmTuhhConstants.BUILDING_PROPERTY_FORMBEIWERT ) ); //$NON-NLS-1$
        dbu.setSecondLine( secLine );
      }
      catch( final Exception e )
      {
        KalypsoCommonsPlugin.getDefault().getLog().log( new Status( IStatus.ERROR, KalypsoCommonsPlugin.getID(), 0, Messages.getString( "org.kalypso.model.wspm.tuhh.core.wspwin.prf.PrfSink.30" ), e ) ); //$NON-NLS-1$
      }
      pw.addDataBlock( dbu );
    }

    else if( buildingTyp.compareTo( IWspmTuhhConstants.BUILDING_TYP_WEHR ) == 0 )
    {
      final DataBlockHeader dbhw = createHeader( "OK-W" ); //$NON-NLS-1$
      final CoordDataBlock dbw = new CoordDataBlock( dbhw );
      writeCoords( profil, profil.hasPointProperty( IWspmTuhhConstants.POINT_PROPERTY_OBERKANTEWEHR ), dbw );
      try
      {
        final Object wehrart = building.getValueFor( IWspmTuhhConstants.BUILDING_PROPERTY_WEHRART );

        final StringBuffer secLine = new StringBuffer( toDataBlockKey( wehrart ) );
        secLine.append( String.format( Locale.US, " %12.4f", building.getValueFor( IWspmTuhhConstants.BUILDING_PROPERTY_FORMBEIWERT ) ) ); //$NON-NLS-1$
        final IProfilPointMarker[] deviders = profil.getPointMarkerFor( profil.hasPointProperty( IWspmTuhhConstants.MARKER_TYP_WEHR ) );
        for( final IProfilPointMarker devider : deviders )
          secLine.append( String.format( Locale.US, " %12.4f", devider.getValue() ) ); //$NON-NLS-1$
        dbw.setSecondLine( secLine.toString() );
      }
      catch( final Exception e )
      {
        KalypsoCommonsPlugin.getDefault().getLog().log( new Status( IStatus.ERROR, KalypsoCommonsPlugin.getID(), 0, Messages.getString( "org.kalypso.model.wspm.tuhh.core.wspwin.prf.PrfSink.34" ), e ) ); //$NON-NLS-1$
      }
      pw.addDataBlock( dbw );
    }
    else if( buildingTyp.compareTo( IWspmTuhhConstants.BUILDING_TYP_EI ) == 0 )
    {
      final DataBlockHeader dbhe = createHeader( "EI" ); //$NON-NLS-1$
      final TextDataBlock dbe = new TextDataBlock( dbhe );
      dbe.setThirdLine( "0  0  0  0  0  0  0  0  8" ); //$NON-NLS-1$
      try
      {
        dbe.addLine( getDoubleStr( building.getValueFor( IWspmTuhhConstants.BUILDING_PROPERTY_BREITE ) ) + getDoubleStr( building.getValueFor( IWspmTuhhConstants.BUILDING_PROPERTY_HOEHE ) )
            + getDoubleStr( building.getValueFor( IWspmTuhhConstants.BUILDING_PROPERTY_SOHLGEFAELLE ) ) + getDoubleStr( building.getValueFor( IWspmTuhhConstants.BUILDING_PROPERTY_BEZUGSPUNKT_X ) )
            + getDoubleStr( building.getValueFor( IWspmTuhhConstants.BUILDING_PROPERTY_BEZUGSPUNKT_Y ) ) );
      }
      catch( final Exception e )
      {
        KalypsoCommonsPlugin.getDefault().getLog().log( new Status( IStatus.ERROR, KalypsoCommonsPlugin.getID(), 0, Messages.getString( "org.kalypso.model.wspm.tuhh.core.wspwin.prf.PrfSink.37" ), e ) ); //$NON-NLS-1$
      }
      pw.addDataBlock( dbe );
    }
    else if( buildingTyp.compareTo( IWspmTuhhConstants.BUILDING_TYP_MAUL ) == 0 )
    {
      final DataBlockHeader dbhm = createHeader( "MAU" ); //$NON-NLS-1$
      final TextDataBlock dbm = new TextDataBlock( dbhm );
      dbm.setThirdLine( "0  0  0  0  0  0  0  0  9" ); //$NON-NLS-1$
      try
      {
        dbm.addLine( getDoubleStr( building.getValueFor( IWspmTuhhConstants.BUILDING_PROPERTY_BREITE ) ) + getDoubleStr( building.getValueFor( IWspmTuhhConstants.BUILDING_PROPERTY_HOEHE ) )
            + getDoubleStr( building.getValueFor( IWspmTuhhConstants.BUILDING_PROPERTY_SOHLGEFAELLE ) ) + getDoubleStr( building.getValueFor( IWspmTuhhConstants.BUILDING_PROPERTY_BEZUGSPUNKT_X ) )
            + getDoubleStr( building.getValueFor( IWspmTuhhConstants.BUILDING_PROPERTY_BEZUGSPUNKT_Y ) ) );
      }
      catch( final Exception e )
      {
        KalypsoCommonsPlugin.getDefault().getLog().log( new Status( IStatus.ERROR, KalypsoCommonsPlugin.getID(), 0, Messages.getString( "org.kalypso.model.wspm.tuhh.core.wspwin.prf.PrfSink.40" ), e ) ); //$NON-NLS-1$
      }
      pw.addDataBlock( dbm );
    }
    else if( buildingTyp.compareTo( IWspmTuhhConstants.BUILDING_TYP_KREIS ) == 0 )
    {
      final DataBlockHeader dbhk = createHeader( "KRE" ); //$NON-NLS-1$
      final TextDataBlock dbk = new TextDataBlock( dbhk );
      dbk.setThirdLine( "0  0  0  0  0  0  0  0  7" ); //$NON-NLS-1$
      try
      {
        dbk.addLine( getDoubleStr( building.getValueFor( IWspmTuhhConstants.BUILDING_PROPERTY_BREITE ) ) + getDoubleStr( building.getValueFor( IWspmTuhhConstants.BUILDING_PROPERTY_SOHLGEFAELLE ) )
            + getDoubleStr( building.getValueFor( IWspmTuhhConstants.BUILDING_PROPERTY_BEZUGSPUNKT_X ) ) + getDoubleStr( building.getValueFor( IWspmTuhhConstants.BUILDING_PROPERTY_BEZUGSPUNKT_Y ) ) );
      }
      catch( final Exception e )
      {
        KalypsoCommonsPlugin.getDefault().getLog().log( new Status( IStatus.ERROR, KalypsoCommonsPlugin.getID(), 0, Messages.getString( "org.kalypso.model.wspm.tuhh.core.wspwin.prf.PrfSink.43" ), e ) ); //$NON-NLS-1$
      }
      pw.addDataBlock( dbk );
    }

    else if( buildingTyp.compareTo( IWspmTuhhConstants.BUILDING_TYP_TRAPEZ ) == 0 )
    {
      final DataBlockHeader dbht = createHeader( "TRA" ); //$NON-NLS-1$
      final TextDataBlock dbt = new TextDataBlock( dbht );
      dbt.setThirdLine( "0  0  0  0  0  0  0  0  6" ); //$NON-NLS-1$
      try
      {
        dbt.addLine( getDoubleStr( building.getValueFor( IWspmTuhhConstants.BUILDING_PROPERTY_BREITE ) ) + getDoubleStr( building.getValueFor( IWspmTuhhConstants.BUILDING_PROPERTY_HOEHE ) )
            + getDoubleStr( building.getValueFor( IWspmTuhhConstants.BUILDING_PROPERTY_STEIGUNG ) ) + getDoubleStr( building.getValueFor( IWspmTuhhConstants.BUILDING_PROPERTY_SOHLGEFAELLE ) )
            + getDoubleStr( building.getValueFor( IWspmTuhhConstants.BUILDING_PROPERTY_BEZUGSPUNKT_X ) ) + getDoubleStr( building.getValueFor( IWspmTuhhConstants.BUILDING_PROPERTY_BEZUGSPUNKT_Y ) ) );
      }
      catch( final Exception e )
      {
        KalypsoCommonsPlugin.getDefault().getLog().log( new Status( IStatus.ERROR, KalypsoCommonsPlugin.getID(), 0, Messages.getString( "org.kalypso.model.wspm.tuhh.core.wspwin.prf.PrfSink.46" ), e ) ); //$NON-NLS-1$
      }
      pw.addDataBlock( dbt );
    }
  }

  private String getDoubleStr( final Object o )
  {

    try
    {
      return Double.valueOf( o.toString() ).isNaN() ? "       0.0000" : String.format( Locale.US, " %12.4f", o ); //$NON-NLS-1$ //$NON-NLS-2$
    }
    catch( final Exception e )
    {
      return "       0.0000"; //$NON-NLS-1$
    }
  }

  private void writeBewuchs( final DataBlockWriter pw, final IProfil profil )
  {
    final DataBlockHeader dbhx = createHeader( "AX" ); //$NON-NLS-1$
    final CoordDataBlock dbx = new CoordDataBlock( dbhx );
    final DataBlockHeader dbhy = createHeader( "AY" ); //$NON-NLS-1$
    final CoordDataBlock dby = new CoordDataBlock( dbhy );
    final DataBlockHeader dbhp = createHeader( "DP" ); //$NON-NLS-1$
    final CoordDataBlock dbp = new CoordDataBlock( dbhp );
    writeCoords( profil, profil.hasPointProperty( IWspmConstants.POINT_PROPERTY_BEWUCHS_AX ), dbx );
    writeCoords( profil, profil.hasPointProperty( IWspmConstants.POINT_PROPERTY_BEWUCHS_AY ), dby );
    writeCoords( profil, profil.hasPointProperty( IWspmConstants.POINT_PROPERTY_BEWUCHS_DP ), dbp );
    pw.addDataBlock( dbx );
    pw.addDataBlock( dby );
    pw.addDataBlock( dbp );
  }

  @SuppressWarnings("unchecked")
  private void extractMetaData( final DataBlockWriter pw, final IProfil p )
  {
    Map<Integer, String[]> metaData;
    try
    {
      metaData = (Map<Integer, String[]>) p.getProperty( "prfFileFormat_MetaData" ); //$NON-NLS-1$
      if( metaData == null )
        metaData = new HashMap<Integer, String[]>();
    }
    catch( final Exception e )
    {
      metaData = new HashMap<Integer, String[]>();
    }
    final String[] line1 = metaData.get( 1 );
    if( line1 == null )
      pw.addKeyValue( 1, new String[] { "", "" } ); //$NON-NLS-1$ //$NON-NLS-2$
    else
      pw.addKeyValue( 1, line1 );
    final String[] line2 = metaData.get( 2 );
    if( line2 == null )
      pw.addKeyValue( 2, new String[] { "", "" } ); //$NON-NLS-1$ //$NON-NLS-2$
    else
      pw.addKeyValue( 2, line2 );
    final String[] line3 = metaData.get( 3 );
    if( line3 == null )
      pw.addKeyValue( 3, new String[] { "STATUS", "" } ); //$NON-NLS-1$ //$NON-NLS-2$
    else
    {
      if( line3[0].length() == 0 )
        line3[0] = "STATUS"; //$NON-NLS-1$
      pw.addKeyValue( 3, line3 );
    }
    final String[] line4 = metaData.get( 4 );
    if( line4 == null )
      pw.addKeyValue( 4, new String[] { "", "" } ); //$NON-NLS-1$ //$NON-NLS-2$
    else
      pw.addKeyValue( 4, line4 );
    final String[] line5 = metaData.get( 5 );
    if( line5 == null )
      pw.addKeyValue( 5, new String[] { "VERZWEIGUNGSKENNUNG", "0" } ); //$NON-NLS-1$ //$NON-NLS-2$
    else
    {
      if( line5[0].length() == 0 )
        line5[0] = "VERZWEIGUNGSKENNUNG"; //$NON-NLS-1$
      pw.addKeyValue( 5, line5 );
    }
    final String[] line6 = metaData.get( 6 );
    if( line6 == null )
      pw.addKeyValue( 6, new String[] { "WASSERSPIEGEL", "" } ); //$NON-NLS-1$ //$NON-NLS-2$
    else
    {
      if( line6[0].length() == 0 )
        line6[0] = "WASSERSPIEGEL"; //$NON-NLS-1$
      pw.addKeyValue( 6, line6 );
    }
    final String[] line7 = metaData.get( 7 );
    if( line7 == null )
      pw.addKeyValue( 7, new String[] { "MEHRFELDBRUECKE", "0" } ); //$NON-NLS-1$ //$NON-NLS-2$
    else
    {
      if( line7[0].length() == 0 )
        line7[0] = "MEHRFELDBRUECKE"; //$NON-NLS-1$
      pw.addKeyValue( 7, line7 );
    }
    final String[] line8 = metaData.get( 8 );
    if( line8 == null )
      pw.addKeyValue( 8, new String[] { "", "" } ); //$NON-NLS-1$ //$NON-NLS-2$
    else
      pw.addKeyValue( 8, line8 );
    final Double d = p.getStation();
    if( d.isNaN() )
      pw.addKeyValue( 9, new String[] { "STATION KM 0.0000", "" } ); //$NON-NLS-1$ //$NON-NLS-2$
    else
      pw.addKeyValue( 9, new String[] { "STATION KM " + String.format( Locale.US, "%.4f", new Object[] { d } ), "" } ); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$

    final String[] line10 = metaData.get( 10 );
    if( line10 == null )
      pw.addKeyValue( 10, new String[] { "", "" } ); //$NON-NLS-1$ //$NON-NLS-2$
    else
      pw.addKeyValue( 10, line10 );
    final String[] line11 = metaData.get( 11 );
    if( line11 == null )
      pw.addKeyValue( 11, new String[] { "", "" } ); //$NON-NLS-1$ //$NON-NLS-2$
    else
      pw.addKeyValue( 11, line11 );
    final String[] line12 = metaData.get( 12 );
    if( line12 == null )
      pw.addKeyValue( 12, new String[] { "", "" } ); //$NON-NLS-1$ //$NON-NLS-2$
    else
      pw.addKeyValue( 12, line12 );
    final String[] line13 = metaData.get( 13 );
    if( line13 == null )
      pw.addKeyValue( 13, new String[] { "", "" } ); //$NON-NLS-1$ //$NON-NLS-2$
    else
      pw.addKeyValue( 13, line13 );
  }

  /**
   * @see org.kalypso.model.wspm.core.profil.serializer.IProfilSink#write(org.kalypso.model.wspm.core.profil.IProfil)
   */
  private boolean internalWrite( final IProfil profil, final Writer writer ) throws IOException
  {
    final DataBlockWriter prfwriter = new DataBlockWriter();
    extractMetaData( prfwriter, profil );
    if( profil.getPoints().length > 0 )
      extractDataBlocks( prfwriter, profil );
    prfwriter.store( new PrintWriter( writer ) );
    return true;
  }

  /**
   * @see org.kalypso.model.wspm.core.profil.serializer.IProfilSink#write(java.lang.Object, java.io.Writer)
   */
  @Override
  public boolean write( final Object source, final Writer writer ) throws IOException
  {
    if( source instanceof IProfil )
      return internalWrite( (IProfil) source, writer );
    if( source instanceof IProfil[] )
      return internalWrite( ((IProfil[]) source)[0], writer );
    throw new IOException( "illegal Argument", new IllegalArgumentException( source.toString() ) );
  }

}
