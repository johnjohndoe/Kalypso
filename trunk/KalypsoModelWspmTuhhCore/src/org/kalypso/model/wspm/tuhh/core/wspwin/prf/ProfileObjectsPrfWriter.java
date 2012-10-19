/** This file is part of Kalypso
 *
 *  Copyright (c) 2012 by
 *
 *  Björnsen Beratende Ingenieure GmbH, Koblenz, Germany (Bjoernsen Consulting Engineers), http://www.bjoernsen.de
 *  Technische Universität Hamburg-Harburg, Institut für Wasserbau, Hamburg, Germany
 *  (Technical University Hamburg-Harburg, Institute of River and Coastal Engineering), http://www.tu-harburg.de/wb/
 *
 *  Kalypso is free software: you can redistribute it and/or modify it under the terms
 *  of the GNU Lesser General Public License (LGPL) as published by the Free Software
 *  Foundation, either version 3 of the License, or (at your option) any later version.
 *
 *  Kalypso is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied
 *  warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public License for more details.
 *
 *  You should have received a copy of the GNU Lesser General Public
 *  License along with Kalypso.  If not, see <http://www.gnu.org/licenses/>.
 */
package org.kalypso.model.wspm.tuhh.core.wspwin.prf;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Locale;

import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.kalypso.commons.KalypsoCommonsPlugin;
import org.kalypso.model.wspm.core.profil.IProfile;
import org.kalypso.model.wspm.core.profil.IProfileObject;
import org.kalypso.model.wspm.core.profil.IProfilePointMarker;
import org.kalypso.model.wspm.core.util.WspmProfileHelper;
import org.kalypso.model.wspm.tuhh.core.IWspmTuhhConstants;
import org.kalypso.model.wspm.tuhh.core.i18n.Messages;
import org.kalypso.model.wspm.tuhh.core.profile.profileobjects.building.BuildingBruecke;
import org.kalypso.model.wspm.tuhh.core.profile.profileobjects.building.BuildingEi;
import org.kalypso.model.wspm.tuhh.core.profile.profileobjects.building.BuildingKreis;
import org.kalypso.model.wspm.tuhh.core.profile.profileobjects.building.BuildingMaul;
import org.kalypso.model.wspm.tuhh.core.profile.profileobjects.building.BuildingTrapez;
import org.kalypso.model.wspm.tuhh.core.profile.profileobjects.building.BuildingWehr;
import org.kalypso.model.wspm.tuhh.core.profile.profileobjects.building.BuildingWehr.WeirType;
import org.kalypso.model.wspm.tuhh.core.profile.profileobjects.building.ICulvertBuilding;
import org.kalypso.model.wspm.tuhh.core.profile.profileobjects.building.IProfileBuilding;
import org.kalypso.model.wspm.tuhh.core.profile.sinuositaet.ISinuositaetProfileObject;
import org.kalypso.model.wspm.tuhh.core.profile.sinuositaet.SINUOSITAET_GERINNE_ART;
import org.kalypso.model.wspm.tuhh.core.profile.sinuositaet.SINUOSITAET_KENNUNG;
import org.kalypso.wspwin.core.prf.DataBlockWriter;
import org.kalypso.wspwin.core.prf.IWspWinConstants;
import org.kalypso.wspwin.core.prf.datablock.CoordDataBlock;
import org.kalypso.wspwin.core.prf.datablock.DataBlockHeader;
import org.kalypso.wspwin.core.prf.datablock.DoubleDataBlock;
import org.kalypso.wspwin.core.prf.datablock.SinuositaetDataBlock;

/**
 * Helper for converting profile objects to .prf.
 *
 * @author Gernot Belger
 */
public class ProfileObjectsPrfWriter
{
  private final IProfile m_profil;

  private boolean m_hasBuildings = false;

  private final DataBlockWriter m_dbWriter;

  public ProfileObjectsPrfWriter( final IProfile profil, final DataBlockWriter dbWriter )
  {
    m_profil = profil;
    m_dbWriter = dbWriter;
  }

  public void write( )
  {
    final IProfileObject[] profileObjects = m_profil.getProfileObjects( IProfileObject.class );

    for( final IProfileObject profileObject : profileObjects )
    {
      if( profileObject instanceof IProfileBuilding )
      {
        /**
         * tuhh profile restriction - only one profile building allowed!
         */
        if( !m_hasBuildings )
          writeBuilding( (IProfileBuilding)profileObject );

        m_hasBuildings = true;
      }
      else
      {
        writeProfileObject( profileObject );
      }
    }
  }

  private void writeBuilding( final IProfileBuilding building )
  {
    if( building instanceof BuildingBruecke )
      writeBridge( (BuildingBruecke)building );
    else if( building instanceof BuildingWehr )
      writeWeir( (BuildingWehr)building );
    else if( building instanceof ICulvertBuilding )
      writeCulvert( (ICulvertBuilding)building );
    else
      throw new UnsupportedOperationException();
  }

  private void writeBridge( final BuildingBruecke brueckeBuilding )
  {
    final DataBlockHeader dbho = PrfHeaders.createHeader( IWspmTuhhConstants.POINT_PROPERTY_OBERKANTEBRUECKE ); //$NON-NLS-1$
    final CoordDataBlock dbo = PrfWriter.writeCoords( m_profil, IWspmTuhhConstants.POINT_PROPERTY_OBERKANTEBRUECKE, dbho, null );
    m_dbWriter.addDataBlock( dbo );

    final DataBlockHeader dbhu = PrfHeaders.createHeader( IWspmTuhhConstants.POINT_PROPERTY_UNTERKANTEBRUECKE ); //$NON-NLS-1$
    final CoordDataBlock dbu = PrfWriter.writeCoords( m_profil, IWspmTuhhConstants.POINT_PROPERTY_UNTERKANTEBRUECKE, dbhu, null );

    try
    {
      final String secLine = String.format( Locale.US, " %12.4f", brueckeBuilding.getUnterwasser() ) //$NON-NLS-1$
          + String.format( Locale.US, " %12.4f", brueckeBuilding.getBreite() ) //$NON-NLS-1$
          + String.format( Locale.US, " %12.4f", brueckeBuilding.getRauheit() ) //$NON-NLS-1$
          + String.format( Locale.US, " %12.4f", brueckeBuilding.getFormbeiwert() ); //$NON-NLS-1$
      dbu.setSecondLine( secLine );
    }
    catch( final Exception e )
    {
      KalypsoCommonsPlugin.getDefault().getLog().log( new Status( IStatus.ERROR, KalypsoCommonsPlugin.getID(), 0, Messages.getString( "org.kalypso.model.wspm.tuhh.core.wspwin.prf.PrfSink.30" ), e ) ); //$NON-NLS-1$
    }

    m_dbWriter.addDataBlock( dbu );
  }

  private void writeWeir( final BuildingWehr building )
  {
    final BuildingWehr wehrBuilding = building;

    final DataBlockHeader dbhw = PrfHeaders.createHeader( IWspmTuhhConstants.POINT_PROPERTY_OBERKANTEWEHR ); //$NON-NLS-1$
    final IProfilePointMarker[] deviders = m_profil.getPointMarkerFor( m_profil.hasPointProperty( IWspmTuhhConstants.MARKER_TYP_WEHR ) );

    CoordDataBlock dbw;

    // FIXME: check, is this really necessary: we interpolate weir values for all weir-markers
    if( deviders.length > 0 )
    {
      final IProfilePointMarker[] trennFl = m_profil.getPointMarkerFor( m_profil.hasPointProperty( IWspmTuhhConstants.MARKER_TYP_TRENNFLAECHE ) );

      final Collection<Double> markedWidths = new ArrayList<>( deviders.length + 2 );
      final Collection<Double> interpolatedWeirHeights = new ArrayList<>( deviders.length + 2 );

      final Double tf0Width = trennFl[0].getPoint().getBreite();
      final Double tf0Height = WspmProfileHelper.interpolateValue( m_profil, tf0Width, IWspmTuhhConstants.POINT_PROPERTY_OBERKANTEWEHR );
      if( tf0Height != null )
      {
        markedWidths.add( tf0Width );
        interpolatedWeirHeights.add( tf0Height );
      }

      /* weir markers */
      for( int i = 0; i < deviders.length - 1; i++ )
      {
        final Double width = deviders[i - 1].getPoint().getBreite();
        final Double height = WspmProfileHelper.interpolateValue( m_profil, width, IWspmTuhhConstants.POINT_PROPERTY_OBERKANTEWEHR );
        if( height != null )
        {
          markedWidths.add( width );
          interpolatedWeirHeights.add( height );
        }
      }

      /* last tf */
      final Double tf1Width = trennFl[trennFl.length - 1].getPoint().getBreite();
      final Double tf1Height = WspmProfileHelper.interpolateValue( m_profil, tf1Width, IWspmTuhhConstants.POINT_PROPERTY_OBERKANTEWEHR );
      if( tf1Height != null )
      {
        markedWidths.add( tf1Width );
        interpolatedWeirHeights.add( tf1Height );
      }

      /* create datablock */
      dbw = new CoordDataBlock( dbhw );

      final Double[] xValues = markedWidths.toArray( new Double[markedWidths.size()] );
      final Double[] yValues = interpolatedWeirHeights.toArray( new Double[interpolatedWeirHeights.size()] );

      dbw.setCoords( xValues, yValues );
    }
    else
    {
      dbw = PrfWriter.writeCoords( m_profil, IWspmTuhhConstants.POINT_PROPERTY_OBERKANTEWEHR, dbhw, null );
    }

    try
    {
      final WeirType wehrart = wehrBuilding.getWehrart();

      final StringBuffer secLine = new StringBuffer( toWeirTypeKey( wehrart ) );
      secLine.append( String.format( Locale.US, " %12.4f", wehrBuilding.getFormbeiwert() ) ); //$NON-NLS-1$
      for( final IProfilePointMarker devider : deviders )
      {
        secLine.append( String.format( Locale.US, " %12.4f", devider.getValue() ) ); //$NON-NLS-1$
      }
      dbw.setSecondLine( secLine.toString() );
    }
    catch( final Exception e )
    {
      KalypsoCommonsPlugin.getDefault().getLog().log( new Status( IStatus.ERROR, KalypsoCommonsPlugin.getID(), 0, Messages.getString( "org.kalypso.model.wspm.tuhh.core.wspwin.prf.PrfSink.34" ), e ) ); //$NON-NLS-1$
    }

    m_dbWriter.addDataBlock( dbw );
  }

  private String toWeirTypeKey( final WeirType wehrart )
  {
    switch( wehrart )
    {
      case beiwert:
        return "BEIWERT"; //$NON-NLS-1$

      case breitkronig:
        return "BREITKRONIG"; //$NON-NLS-1$

      case rundkronig:
        return "RUNDKRONIG"; //$NON-NLS-1$

      case scharfkantig:
        return "SCHARFKANTIG"; //$NON-NLS-1$
    }

    throw new IllegalStateException();
  }

  private void writeCulvert( final ICulvertBuilding culvert )
  {
    final DataBlockHeader dbh = PrfHeaders.createHeader( culvert.getType() );
    final DoubleDataBlock db = new DoubleDataBlock( dbh );

    try
    {
      final Double[] values = getCulvertValues( culvert );
      db.setDoubles( values );
    }
    catch( final Exception e )
    {
      final String message = Messages.getString( "org.kalypso.model.wspm.tuhh.core.wspwin.prf.PrfSink.37" ); //$NON-NLS-1$
      final IStatus status = new Status( IStatus.ERROR, KalypsoCommonsPlugin.getID(), message, e );
      KalypsoCommonsPlugin.getDefault().getLog().log( status );
    }

    m_dbWriter.addDataBlock( db );
  }

  private Double[] getCulvertValues( final ICulvertBuilding culvert )
  {
    if( culvert instanceof BuildingEi )
      return writeCulvertOval( (BuildingEi)culvert );

    if( culvert instanceof BuildingMaul )
      return writeCulvertMaul( (BuildingMaul)culvert );

    if( culvert instanceof BuildingKreis )
      return writeCulvertCircle( (BuildingKreis)culvert );

    if( culvert instanceof BuildingTrapez )
      return writeCulvertTrapezoid( (BuildingTrapez)culvert );

    throw new UnsupportedOperationException();
  }

  private Double[] writeCulvertOval( final BuildingEi eiBuilding )
  {
    return new Double[] { eiBuilding.getBreite(), eiBuilding.getHoehe(), eiBuilding.getSohlgefaelle(), eiBuilding.getBezugspunktX(), eiBuilding.getBezugspunktY() };
  }

  private Double[] writeCulvertMaul( final BuildingMaul maulBuilding )
  {
    return new Double[] { maulBuilding.getBreite(), maulBuilding.getHoehe(), maulBuilding.getSohlgefaelle(), maulBuilding.getBezugspunktX(), maulBuilding.getBezugspunktY() };
  }

  private Double[] writeCulvertCircle( final BuildingKreis kreisBuilding )
  {
    return new Double[] { kreisBuilding.getBreite(), kreisBuilding.getSohlgefaelle(), kreisBuilding.getBezugspunktX(), kreisBuilding.getBezugspunktY() };
  }

  private Double[] writeCulvertTrapezoid( final BuildingTrapez trapezBuilding )
  {
    return new Double[] { trapezBuilding.getBreite(), trapezBuilding.getHoehe(), trapezBuilding.getSteigung(), trapezBuilding.getSohlgefaelle(), trapezBuilding.getBezugspunktX(),
        trapezBuilding.getBezugspunktY() };
  }

  private void writeProfileObject( final IProfileObject profileObject )
  {
    if( profileObject instanceof ISinuositaetProfileObject )
    {
      final ISinuositaetProfileObject sinuosity = (ISinuositaetProfileObject)profileObject;

      final DataBlockHeader header = new DataBlockHeader( IPrfConstants.HEADER_SINUOSITAET, "[-]", IWspWinConstants.SPEZIALPROFIL_SINUOSITAET );//$NON-NLS-1$ //$NON-NLS-2$

      final DoubleDataBlock dataBlock = new SinuositaetDataBlock( header );
      final SINUOSITAET_KENNUNG kennung = sinuosity.getKennung();
      final double sinus = sinuosity.getSn();
      final SINUOSITAET_GERINNE_ART gerinne = sinuosity.getGerinneArt();
      final double lf = sinuosity.getLf();

      dataBlock.setDoubles( new Double[] { new Double( kennung.toInteger() ), sinus, new Double( gerinne.toInteger() ), lf } );

      m_dbWriter.addDataBlock( dataBlock );
    }
  }
}