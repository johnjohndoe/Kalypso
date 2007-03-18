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
package org.kalypso.model.wspm.tuhh.core.wspwin.prf;

import java.io.PrintWriter;
import java.io.Writer;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.Locale;
import java.util.Map;
import java.util.logging.Level;
import java.util.logging.Logger;

import org.kalypso.model.wspm.core.profil.IProfil;
import org.kalypso.model.wspm.core.profil.IProfilPoint;
import org.kalypso.model.wspm.core.profil.IProfilPointMarker;
import org.kalypso.model.wspm.core.profil.IProfileObject;
import org.kalypso.model.wspm.core.profil.serializer.IProfilSink;
import org.kalypso.model.wspm.tuhh.core.IWspmTuhhConstants;
import org.kalypso.wspwin.core.prf.PrfWriter;
import org.kalypso.wspwin.core.prf.datablock.CoordDataBlock;
import org.kalypso.wspwin.core.prf.datablock.DataBlockHeader;
import org.kalypso.wspwin.core.prf.datablock.TextDataBlock;

/**
 * @author kimwerner
 */
public class PrfSink implements IProfilSink
{

  private final static Logger m_logger = Logger.getLogger( PrfSink.class.getName() );

  private String toDataBlockKey( final Object profilKey )
  {
    final String value = profilKey.toString();
    if( value.compareTo( IWspmTuhhConstants.WEHR_TYP_BEIWERT ) == 0 )
      return "BEIWERT";
    else if( value.compareTo( IWspmTuhhConstants.WEHR_TYP_RUNDKRONIG ) == 0 )
      return "RUNDKRONIG";
    else if( value.compareTo( IWspmTuhhConstants.WEHR_TYP_SCHARFKANTIG ) == 0 )
      return "SCHARFKANTIG";
    else if( value.compareTo( IWspmTuhhConstants.WEHR_TYP_BREITKRONIG ) == 0 )
      return "BREITKRONIG";
    else
      return value;
  }

  private void extractDataBlocks( final PrfWriter pw, final IProfil p )
  {
    final IProfilPoint anyPoint = p.getPoints().getFirst();
    writePoints( pw, p );
    writeDevider( pw, p );
    if( anyPoint.hasProperty( IWspmTuhhConstants.POINT_PROPERTY_RAUHEIT) )
      writeRauheit( pw, p );
    if( p.getProfileObject() != null )
      writeBuilding( pw, p );
    if( anyPoint.hasProperty( IWspmTuhhConstants.POINT_PROPERTY_HOCHWERT ) )
      writeHochRechts( pw, p );
    if( anyPoint.hasProperty( IWspmTuhhConstants.POINT_PROPERTY_BEWUCHS_AX ) )
      writeBewuchs( pw, p );
  }

  private void writePoints( final PrfWriter pw, final IProfil profil )
  {
    final DataBlockHeader dbh = PrfWriter.createHeader( "GEL" );
    final CoordDataBlock db = new CoordDataBlock( dbh );
    writeCoords( profil, IWspmTuhhConstants.POINT_PROPERTY_HOEHE, db );
    pw.addDataBlock( db );
  }

  private void writeRauheit( final PrfWriter pw, final IProfil profil )
  {
    final DataBlockHeader dbhr = PrfWriter.createHeader( "RAU" );
    final CoordDataBlock dbr = new CoordDataBlock( dbhr );
    if( IWspmTuhhConstants.RAUHEIT_TYP_KST.equals( profil.getProperty( IWspmTuhhConstants.RAUHEIT_TYP )) )
    {
      dbr.setSecondLine( "kst   m" );
    }
    else
    {
      dbr.setSecondLine( "k-s   m" );
    }
    writeCoords( profil, IWspmTuhhConstants.POINT_PROPERTY_RAUHEIT, dbr );
    final IProfileObject building = profil.getProfileObject();
    final String buildingTyp = building == null ? "" : building.getId();
    if( buildingTyp.equals( IWspmTuhhConstants.BUILDING_TYP_BRUECKE ) || buildingTyp.equals( IWspmTuhhConstants.BUILDING_TYP_WEHR ))
    {
      try
      {
        dbr.getY()[0] = (Double) building.getValueFor(IWspmTuhhConstants.BUILDING_PROPERTY_RAUHEIT );
      }
      catch( Exception e )
      {
        m_logger.log( Level.SEVERE, "Der Rauheitswert für das Bauwerk konnte nicht geschrieben werden." );
      }
    }
    pw.addDataBlock( dbr );
  }

  private void writeCoords( final IProfil profil, final String prop, final CoordDataBlock db )
  {
    final LinkedList<IProfilPoint> points = profil.getPoints();
    final double[] Xs = new double[points.size()];
    final double[] Ys = new double[points.size()];
    int index = 0;
    for( final IProfilPoint point : points )
    {
      try
      {
        Xs[index] = point.getValueFor( IWspmTuhhConstants.POINT_PROPERTY_BREITE );
        Ys[index] = profil.hasPointProperty( prop ) ? point.getValueFor( prop ) : 0.0;
      }
      catch(Exception e )
      {
        Xs[index] = 0;
        Ys[index] = 0;
        m_logger.log( Level.SEVERE, prop.toString() + " an Position " + Integer.toString( index ) + " konnte nicht geschrieben werden." );
      }
      index++;
    }
    db.setCoords( Xs, Ys );
  }

  private void writeHochRechts( final PrfWriter pw, final IProfil profil )
  {

    final DataBlockHeader dbhh = PrfWriter.createHeader( "HOC" );
    final CoordDataBlock dbh = new CoordDataBlock( dbhh );
    writeCoords( profil, IWspmTuhhConstants.POINT_PROPERTY_HOCHWERT, dbh );
    pw.addDataBlock( dbh );

    final DataBlockHeader dbhr = PrfWriter.createHeader( "REC" );
    final CoordDataBlock dbr = new CoordDataBlock( dbhr );
    writeCoords( profil, IWspmTuhhConstants.POINT_PROPERTY_RECHTSWERT, dbr );
    pw.addDataBlock( dbr );
  }

  private void writeDevider( final PrfWriter pw, final IProfil profil )
  {

    final IProfilPointMarker[] trennf = profil.getPointMarkerFor( IWspmTuhhConstants.MARKER_TYP_TRENNFLAECHE );
    if( trennf.length > 0 )
    {
      final DataBlockHeader dbht = PrfWriter.createHeader( "TRENNF" );
      final CoordDataBlock dbt = new CoordDataBlock( dbht );
      final double[] xs = new double[trennf.length];
      final double[] ys = new double[trennf.length];
      int index = 0;
      for( IProfilPointMarker devider : trennf )
      {
        final IProfilPoint point = devider.getPoint();
        try
        {
          xs[index] = point.getValueFor( IWspmTuhhConstants.POINT_PROPERTY_BREITE );
        }
        catch( Exception e )
        {
          m_logger.log( Level.SEVERE, "Die Positionen der Trennflächen konnten nicht geschrieben werden." );

        }

        boolean isBoeschung = devider.getValueFor(IWspmTuhhConstants.POINTMARKER_PROPERTY_BOESCHUNG ) == null ? false : (Boolean) devider.getValueFor( IWspmTuhhConstants.POINTMARKER_PROPERTY_BOESCHUNG );
        switch( index )
        {
          case 0:
            ys[0] = isBoeschung ? 3.0 : 1.0;
            break;
          case 1:
            ys[1] = isBoeschung ? 4.0 : 2.0;
            break;
          default:
            ys[index] = 0.0;
        }
        index++;
      }
      dbt.setCoords( xs, ys );
      pw.addDataBlock( dbt );
    }
    writeDeviderTyp( pw, "BOR", profil.getPointMarkerFor( IWspmTuhhConstants.MARKER_TYP_BORDVOLL ) );
    writeDeviderTyp( pw, "DUR", profil.getPointMarkerFor( IWspmTuhhConstants.MARKER_TYP_DURCHSTROEMTE ) );
    writeDeviderTyp( pw, "TRENNL", profil.getPointMarkerFor( IWspmTuhhConstants.MARKER_TYP_WEHR ) );
  }

  private void writeDeviderTyp( final PrfWriter pw, final String key, IProfilPointMarker[] deviders )
  {

    if( deviders != null && deviders.length > 0 )
    {
      final DataBlockHeader dbhw = PrfWriter.createHeader( key );
      final CoordDataBlock dbw = new CoordDataBlock( dbhw );
      final double[] xs = new double[deviders.length];
      final double[] ys = new double[deviders.length];
      int index = 0;
      for( IProfilPointMarker devider : deviders )
      {
        final IProfilPoint point = devider.getPoint();
        try
        {
          xs[index] = point.getValueFor(IWspmTuhhConstants.POINT_PROPERTY_BREITE );
          ys[index] = point.getValueFor(IWspmTuhhConstants.POINT_PROPERTY_HOEHE );
        }
        catch(Exception e )
        {
          m_logger.log( Level.SEVERE, "Die Positionen der " + devider.getMarkerId().toString() + " konnten nicht geschrieben werden." );

        }
        index++;
      }
      dbw.setCoords( xs, ys );
      pw.addDataBlock( dbw );
    }
  }

  private void writeBuilding( final PrfWriter pw, final IProfil profil )
  {
    final IProfileObject building = profil.getProfileObject();
    final String buildingTyp = building == null ? "" : building.getId();
    if( buildingTyp.equals( IWspmTuhhConstants.BUILDING_TYP_BRUECKE ) )
    {
      final DataBlockHeader dbho = PrfWriter.createHeader( "OK-B" );
      final CoordDataBlock dbo = new CoordDataBlock( dbho );
      writeCoords( profil, IWspmTuhhConstants.POINT_PROPERTY_OBERKANTEBRUECKE, dbo );
      pw.addDataBlock( dbo );
      final DataBlockHeader dbhu = PrfWriter.createHeader( "UK-B" );
      final CoordDataBlock dbu = new CoordDataBlock( dbhu );
      writeCoords( profil, IWspmTuhhConstants.POINT_PROPERTY_UNTERKANTEBRUECKE, dbu );
      try
      {
        final String secLine = String.format( Locale.US, " %12.4f", building.getValueFor( IWspmTuhhConstants.BUILDING_PROPERTY_UNTERWASSER ) )
            + String.format( Locale.US, " %12.4f", building.getValueFor( IWspmTuhhConstants.BUILDING_PROPERTY_BREITE ) ) + String.format( Locale.US, " %12.4f", building.getValueFor( IWspmTuhhConstants.BUILDING_PROPERTY_RAUHEIT ) )
            + String.format( Locale.US, " %12.4f", building.getValueFor( IWspmTuhhConstants.BUILDING_PROPERTY_FORMBEIWERT ) );
        dbu.setSecondLine( secLine );
      }
      catch( final Exception e )
      {
        m_logger.log( Level.SEVERE, "Fehler beim schreiben der Brückenparameter" );
      }
      pw.addDataBlock( dbu );
    }

    else if( buildingTyp.compareTo( IWspmTuhhConstants.BUILDING_TYP_WEHR ) == 0 )
    {
      final DataBlockHeader dbhw = PrfWriter.createHeader( "OK-W" );
      final CoordDataBlock dbw = new CoordDataBlock( dbhw );
      writeCoords( profil,IWspmTuhhConstants.POINT_PROPERTY_OBERKANTEWEHR, dbw );
      try
      {
        final Object wehrart = building.getValueFor( IWspmTuhhConstants.BUILDING_PROPERTY_WEHRART );
        final StringBuffer secLine = new StringBuffer( toDataBlockKey( wehrart ) );
        secLine.append( String.format( Locale.US, " %12.4f", building.getValueFor( IWspmTuhhConstants.BUILDING_PROPERTY_FORMBEIWERT ) ) );
        final IProfilPointMarker[] deviders = profil.getPointMarkerFor( IWspmTuhhConstants.MARKER_TYP_WEHR );
        for( IProfilPointMarker devider : deviders )
        {
          secLine.append( String.format( Locale.US, " %12.4f", devider.getValueFor(IWspmTuhhConstants.POINTMARKER_PROPERTY_BEIWERT ) ) );
        }
        dbw.setSecondLine( secLine.toString() );
      }
      catch( final Exception e )
      {
        m_logger.log( Level.SEVERE, "Fehler beim schreiben der Wehrparameter" );
      }
      pw.addDataBlock( dbw );
    }
    else if( buildingTyp.compareTo( IWspmTuhhConstants.BUILDING_TYP_EI ) == 0 )
    {
      final DataBlockHeader dbhe = PrfWriter.createHeader( "EI" );
      final TextDataBlock dbe = new TextDataBlock( dbhe );
      dbe.setThirdLine( "0  0  0  0  0  0  0  0  8" );
      try
      {
        dbe.addLine( getDoubleStr( building.getValueFor( IWspmTuhhConstants.BUILDING_PROPERTY_BREITE ) ) + getDoubleStr( building.getValueFor( IWspmTuhhConstants.BUILDING_PROPERTY_HOEHE ) )
            + getDoubleStr( building.getValueFor( IWspmTuhhConstants.BUILDING_PROPERTY_SOHLGEFAELLE ) ) + getDoubleStr( building.getValueFor( IWspmTuhhConstants.BUILDING_PROPERTY_BEZUGSPUNKT_X ) )
            + getDoubleStr( building.getValueFor( IWspmTuhhConstants.BUILDING_PROPERTY_BEZUGSPUNKT_Y ) ) );
      }
      catch( final Exception e )
      {
        m_logger.log( Level.SEVERE, "Fehler beim schreiben der Bauwerksparameter" );
      }
      pw.addDataBlock( dbe );
    }
    else if( buildingTyp.compareTo( IWspmTuhhConstants.BUILDING_TYP_MAUL ) == 0 )
    {
      final DataBlockHeader dbhm = PrfWriter.createHeader( "MAU" );
      final TextDataBlock dbm = new TextDataBlock( dbhm );
      dbm.setThirdLine( "0  0  0  0  0  0  0  0  9" );
      try
      {
        dbm.addLine( getDoubleStr( building.getValueFor(IWspmTuhhConstants.BUILDING_PROPERTY_BREITE ) ) + getDoubleStr( building.getValueFor( IWspmTuhhConstants.BUILDING_PROPERTY_HOEHE ) )
            + getDoubleStr( building.getValueFor( IWspmTuhhConstants.BUILDING_PROPERTY_SOHLGEFAELLE ) ) + getDoubleStr( building.getValueFor(IWspmTuhhConstants.BUILDING_PROPERTY_BEZUGSPUNKT_X ) )
            + getDoubleStr( building.getValueFor( IWspmTuhhConstants.BUILDING_PROPERTY_BEZUGSPUNKT_Y ) ) );
      }
      catch( final Exception e )
      {
        m_logger.log( Level.SEVERE, "Fehler beim schreiben der Bauwerksparameter" );
      }
      pw.addDataBlock( dbm );
    }
    else if( buildingTyp.compareTo( IWspmTuhhConstants.BUILDING_TYP_KREIS ) == 0 )
    {
      final DataBlockHeader dbhk = PrfWriter.createHeader( "KRE" );
      final TextDataBlock dbk = new TextDataBlock( dbhk );
      dbk.setThirdLine( "0  0  0  0  0  0  0  0  7" );
      try
      {
        dbk.addLine( getDoubleStr( building.getValueFor( IWspmTuhhConstants.BUILDING_PROPERTY_BREITE ) ) + getDoubleStr( building.getValueFor( IWspmTuhhConstants.BUILDING_PROPERTY_SOHLGEFAELLE ) )
            + getDoubleStr( building.getValueFor( IWspmTuhhConstants.BUILDING_PROPERTY_BEZUGSPUNKT_X ) ) + getDoubleStr( building.getValueFor( IWspmTuhhConstants.BUILDING_PROPERTY_BEZUGSPUNKT_Y ) ) );
      }
      catch( final Exception e )
      {
        m_logger.log( Level.SEVERE, "Fehler beim schreiben der Bauwerksparameter" );
      }
      pw.addDataBlock( dbk );
    }

    else if( buildingTyp.compareTo( IWspmTuhhConstants.BUILDING_TYP_TRAPEZ ) == 0 )
    {
      final DataBlockHeader dbht = PrfWriter.createHeader( "TRA" );
      final TextDataBlock dbt = new TextDataBlock( dbht );
      dbt.setThirdLine( "0  0  0  0  0  0  0  0  6" );
      try
      {
        dbt.addLine( getDoubleStr( building.getValueFor( IWspmTuhhConstants.BUILDING_PROPERTY_BREITE ) ) + getDoubleStr( building.getValueFor( IWspmTuhhConstants.BUILDING_PROPERTY_HOEHE ) )
            + getDoubleStr( building.getValueFor( IWspmTuhhConstants.BUILDING_PROPERTY_STEIGUNG ) ) + getDoubleStr( building.getValueFor( IWspmTuhhConstants.BUILDING_PROPERTY_SOHLGEFAELLE ) )
            + getDoubleStr( building.getValueFor( IWspmTuhhConstants.BUILDING_PROPERTY_BEZUGSPUNKT_X ) ) + getDoubleStr( building.getValueFor(IWspmTuhhConstants.BUILDING_PROPERTY_BEZUGSPUNKT_Y ) ) );
      }
      catch( final Exception e )
      {
        m_logger.log( Level.SEVERE, "Fehler beim schreiben der Bauwerksparameter" );
      }
      pw.addDataBlock( dbt );
    }
  }

  private String getDoubleStr( final Object o )
  {
    try
    {
      return String.format( Locale.US, " %12.4f", o );
    }
    catch( Exception e )
    {
      return "       0.0000";
    }
  }

  private void writeBewuchs( final PrfWriter pw, final IProfil profil )
  {
    final DataBlockHeader dbhx = PrfWriter.createHeader( "AX" );
    final CoordDataBlock dbx = new CoordDataBlock( dbhx );
    final DataBlockHeader dbhy = PrfWriter.createHeader( "AY" );
    final CoordDataBlock dby = new CoordDataBlock( dbhy );
    final DataBlockHeader dbhp = PrfWriter.createHeader( "DP" );
    final CoordDataBlock dbp = new CoordDataBlock( dbhp );
    writeCoords( profil, IWspmTuhhConstants.POINT_PROPERTY_BEWUCHS_AX, dbx );
    writeCoords( profil, IWspmTuhhConstants.POINT_PROPERTY_BEWUCHS_AY, dby );
    writeCoords( profil, IWspmTuhhConstants.POINT_PROPERTY_BEWUCHS_DP, dbp );
    pw.addDataBlock( dbx );
    pw.addDataBlock( dby );
    pw.addDataBlock( dbp );
  }

  @SuppressWarnings("unchecked")
  private void extractMetaData( final PrfWriter pw, final IProfil p )

  {

    Map<Integer, String[]> metaData;
    try
    {
      metaData = (Map<Integer, String[]>) p.getProperty( "prfFileFormat_MetaData" );
      if( metaData == null )
        metaData = new HashMap<Integer, String[]>();
    }
    catch( Exception e )
    {
      metaData = new HashMap<Integer, String[]>();
    }
    final String[] line1 = metaData.get( 1 );
    if( line1 == null )
    {
      pw.addKeyValue( 1, new String[] { "", "" } );
    }
    else
    {
      pw.addKeyValue( 1, line1 );
    }
    final String[] line2 = metaData.get( 2 );
    if( line2 == null )
    {
      pw.addKeyValue( 2, new String[] { "", "" } );
    }
    else
    {
      pw.addKeyValue( 2, line2 );
    }
    final String[] line3 = metaData.get( 3 );
    if( line3 == null )
    {
      pw.addKeyValue( 3, new String[] { "STATUS", "" } );
    }
    else
    {
      if( line3[0].length() == 0 )
        line3[0] = "STATUS";
      pw.addKeyValue( 3, line3 );
    }
    final String[] line4 = metaData.get( 4 );
    if( line4 == null )
    {
      pw.addKeyValue( 4, new String[] { "", "" } );
    }
    else
    {
      pw.addKeyValue( 4, line4 );
    }
    final String[] line5 = metaData.get( 5 );
    if( line5 == null )
    {
      pw.addKeyValue( 5, new String[] { "VERZWEIGUNGSKENNUNG", "0" } );
    }
    else
    {
      if( line5[0].length() == 0 )
        line5[0] = "VERZWEIGUNGSKENNUNG";
      pw.addKeyValue( 5, line5 );
    }
    final String[] line6 = metaData.get( 6 );
    if( line6 == null )
    {
      pw.addKeyValue( 6, new String[] { "WASSERSPIEGEL", "" } );
    }
    else
    {
      if( line6[0].length() == 0 )
        line6[0] = "WASSERSPIEGEL";
      pw.addKeyValue( 6, line6 );
    }
    final String[] line7 = metaData.get( 7 );
    if( line7 == null )
    {
      pw.addKeyValue( 7, new String[] { "MEHRFELDBRUECKE", "0" } );
    }
    else
    {
      if( line7[0].length() == 0 )
        line7[0] = "MEHRFELDBRUECKE";
      pw.addKeyValue( 7, line7 );
    }
    final String[] line8 = metaData.get( 8 );
    if( line8 == null )
    {
      pw.addKeyValue( 8, new String[] { "", "" } );
    }
    else
    {
      pw.addKeyValue( 8, line8 );
    }
    final Double d = p.getStation();
    if( d.isNaN() )
      pw.addKeyValue( 9, new String[] { "STATION KM 0.0000", "" } );
    else
      pw.addKeyValue( 9, new String[] { "STATION KM " + String.format( Locale.US, "%.4f", new Object[] { d } ), "" } );

    final String[] line10 = metaData.get( 10 );
    if( line10 == null )
    {
      pw.addKeyValue( 10, new String[] { "", "" } );
    }
    else
    {
      pw.addKeyValue( 10, line10 );
    }
    final String[] line11 = metaData.get( 11 );
    if( line11 == null )
    {
      pw.addKeyValue( 11, new String[] { "", "" } );
    }
    else
    {
      pw.addKeyValue( 11, line11 );
    }
    final String[] line12 = metaData.get( 12 );
    if( line12 == null )
    {
      pw.addKeyValue( 12, new String[] { "", "" } );
    }
    else
    {
      pw.addKeyValue( 12, line12 );
    }
    final String[] line13 = metaData.get( 13 );
    if( line13 == null )
    {
      pw.addKeyValue( 13, new String[] { "", "" } );
    }
    else
    {
      pw.addKeyValue( 13, line13 );
    }

  }

  /**
   * @see org.kalypso.model.wspm.core.profil.serializer.IProfilSink#write(org.kalypso.model.wspm.core.profil.IProfil)
   */
  public void write( final IProfil profil, final Writer writer )
  {
    final PrintWriter pw = new PrintWriter( writer );

    final PrfWriter prfwriter = new PrfWriter();
    extractMetaData( prfwriter, profil );
    if( !profil.getPoints().isEmpty() )
      extractDataBlocks( prfwriter, profil );
    prfwriter.store( pw );
  }

}