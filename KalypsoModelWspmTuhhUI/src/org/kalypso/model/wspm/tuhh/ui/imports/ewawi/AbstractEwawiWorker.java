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
package org.kalypso.model.wspm.tuhh.ui.imports.ewawi;

import java.text.DateFormat;
import java.util.Date;

import org.eclipse.core.runtime.CoreException;
import org.kalypso.model.wspm.core.gml.IProfileFeature;
import org.kalypso.model.wspm.ewawi.data.EwawiPro;
import org.kalypso.model.wspm.ewawi.data.EwawiSta;
import org.kalypso.model.wspm.ewawi.data.enums.EwawiObjectart;
import org.kalypso.model.wspm.ewawi.data.enums.EwawiProfilart;
import org.kalypso.model.wspm.ewawi.utils.EwawiException;
import org.kalypso.model.wspm.ewawi.utils.GewShape;
import org.kalypso.model.wspm.ewawi.utils.profiles.EwawiProfile;
import org.kalypso.model.wspm.ewawi.utils.profiles.EwawiProfilePart;
import org.kalypso.model.wspm.tuhh.ui.i18n.Messages;
import org.kalypso.shape.dbf.DBaseException;
import org.kalypso.shape.deegree.SHP2GM_Object;
import org.kalypso.shape.geometry.ISHPGeometry;
import org.kalypso.shape.geometry.SHPPolyLinez;
import org.kalypso.shape.tools.SHP2JTS;
import org.kalypsodeegree.model.geometry.GM_Curve;
import org.kalypsodeegree.model.geometry.GM_MultiCurve;
import org.kalypsodeegree.model.geometry.GM_Object;

import com.vividsolutions.jts.geom.Geometry;
import com.vividsolutions.jts.geom.GeometryFactory;

/**
 * @author Holger Albert
 */
public abstract class AbstractEwawiWorker
{
  public AbstractEwawiWorker( )
  {
  }

  public abstract void updateClassifications( ) throws Exception;

  public abstract IProfileFeature createNewProfile( EwawiImportData data, final GewShape gewShape, final EwawiSta staIndex, final EwawiProfile ewawiProfile ) throws CoreException;

  public abstract void createMarkers( IProfileFeature profileFeature );

  public abstract void updateWaterLevelFixation( EwawiSta staIndex, EwawiPro proIndex ) throws EwawiException;

  public abstract void fireChangeEvents( );

  protected String getName( final EwawiSta staIndex, final EwawiProfilePart basePart ) throws EwawiException
  {
    final Short profilNummer = basePart.getProfilNummer( staIndex );
    final EwawiProfilart profilArt = basePart.getProfilArt( staIndex );

    if( profilNummer != null )
      return String.format( "%d (%s)", profilNummer, profilArt.getLabel() ); //$NON-NLS-1$

    return String.format( "NNN (%s)", profilArt.getLabel() ); //$NON-NLS-1$
  }

  protected String getDescription( final EwawiSta staIndex, final EwawiProfilePart basePart ) throws EwawiException
  {
    final StringBuilder description = new StringBuilder();

    final EwawiObjectart objectArt = basePart.getObjectArt( staIndex );
    if( objectArt != null && objectArt != EwawiObjectart._1100 )
    {
      final String objectArtText = String.format( Messages.getString( "AbstractEwawiWorker.2" ), objectArt.getKey(), objectArt.getLabel() ); //$NON-NLS-1$
      description.append( objectArtText );
    }

    final Short zusatz = basePart.getZusatz( staIndex );
    if( zusatz != null && zusatz != 0 )
    {
      final String zusatzText = String.format( Messages.getString( "AbstractEwawiWorker.3" ), zusatz ); //$NON-NLS-1$
      description.append( zusatzText );
    }

    final Date validity = basePart.getValidity( staIndex );
    if( validity != null )
    {
      final DateFormat df = DateFormat.getDateInstance( DateFormat.MEDIUM );
      final String validityText = String.format( Messages.getString( "AbstractEwawiWorker.4" ), df.format( validity ) ); //$NON-NLS-1$
      description.append( validityText );
    }

    final EwawiProfilart profilArt = basePart.getProfilArt( staIndex );
    if( profilArt != null )
    {
      final String profilArtText = String.format( Messages.getString( "AbstractEwawiWorker.5" ), profilArt.getKey(), profilArt.getLabel() ); //$NON-NLS-1$
      description.append( profilArtText );
    }

    final String comment = basePart.getComment( staIndex );
    if( comment != null && !comment.equals( "-" ) ) //$NON-NLS-1$
    {
      final String commentText = String.format( Messages.getString( "AbstractEwawiWorker.7" ), comment ); //$NON-NLS-1$
      description.append( commentText );
    }

    return description.toString();
  }

  protected String getRiverId( final EwawiProfilePart basePart )
  {
    final Long gewKennzahl = basePart.getGewKennzahl();
    if( gewKennzahl == null )
      return "-1"; //$NON-NLS-1$

    return String.format( "%d", gewKennzahl ); //$NON-NLS-1$
  }

  protected String getRiverName( final EwawiImportData data, final GewShape gewShape, final EwawiSta staIndex, final EwawiProfilePart basePart ) throws DBaseException, EwawiException
  {
    if( gewShape == null )
      return Messages.getString( "AbstractEwawiWorker.10" ); //$NON-NLS-1$

    final Long gewKennzahl = basePart.getGewKennzahl();
    if( gewKennzahl == null )
      return Messages.getString( "AbstractEwawiWorker.10" ); //$NON-NLS-1$

    final SHPPolyLinez shape = basePart.getShape( staIndex );
    final SHP2JTS shp2jts = new SHP2JTS( new GeometryFactory() );
    final Geometry geometry = shp2jts.transform( shape );

    final String name = (String)gewShape.getValue( gewKennzahl, data.getRiverShapeData().getRiverNameField(), geometry );
    if( name == null )
      return Messages.getString( "AbstractEwawiWorker.10" ); //$NON-NLS-1$

    return name;
  }

  protected GM_Curve getRiverGeometry( final GewShape gewShape, final EwawiSta staIndex, final EwawiProfilePart basePart ) throws EwawiException
  {
    if( gewShape == null )
      return null;

    final Long gewKennzahl = basePart.getGewKennzahl();
    if( gewKennzahl == null )
      return null;

    final SHPPolyLinez shape = basePart.getShape( staIndex );

    final SHP2JTS shp2jts = new SHP2JTS( new GeometryFactory() );
    final Geometry geometry = shp2jts.transform( shape );

    final ISHPGeometry riverShape = gewShape.getShape( gewKennzahl, geometry );

    final GM_Object riverline = SHP2GM_Object.transform( null, riverShape );
    if( riverline instanceof GM_Curve )
      return (GM_Curve)riverline;

    if( riverline instanceof GM_MultiCurve )
    {
      final GM_MultiCurve multiCurve = (GM_MultiCurve)riverline;
      if( multiCurve.getSize() > 1 )
      {
        System.out.println( "Got a multi curve mit more than one curve..." ); //$NON-NLS-1$
        return multiCurve.getCurveAt( 0 );
      }

      if( multiCurve.getSize() == 1 )
        return multiCurve.getCurveAt( 0 );

      return null;
    }

    throw new IllegalStateException( "Geometry of the river shape must be a curve..." ); //$NON-NLS-1$
  }
}