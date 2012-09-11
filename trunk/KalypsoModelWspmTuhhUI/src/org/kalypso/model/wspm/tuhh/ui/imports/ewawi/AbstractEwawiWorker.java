/** This file is part of Kalypso
 *
 *  Copyright (c) 2012 by
 *
 *  Bj�rnsen Beratende Ingenieure GmbH, Koblenz, Germany (Bjoernsen Consulting Engineers), http://www.bjoernsen.de
 *  Technische Universit�t Hamburg-Harburg, Institut f�r Wasserbau, Hamburg, Germany
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
import org.kalypso.shape.dbf.DBaseException;
import org.kalypsodeegree.model.geometry.GM_Curve;

/**
 * @author Holger Albert
 */
public abstract class AbstractEwawiWorker
{
  public AbstractEwawiWorker( )
  {
  }

  public abstract void updateClassifications( );

  public abstract IProfileFeature createNewProfile( EwawiImportData data, final GewShape gewShape, final EwawiSta staIndex, final EwawiProfile ewawiProfile ) throws CoreException;

  public abstract void createMarkers( IProfileFeature profileFeature );

  public abstract void updateWaterLevelFixation( EwawiSta staIndex, EwawiPro proIndex ) throws EwawiException;

  public abstract void fireChangeEvents( );

  protected String getName( final EwawiSta staIndex, final EwawiProfilePart basePart ) throws EwawiException
  {
    final Short profilNummer = basePart.getProfilNummer( staIndex );
    final EwawiProfilart profilArt = basePart.getProfilArt( staIndex );

    if( profilNummer != null )
      return String.format( "%d (%s)", profilNummer, profilArt.getLabel() );

    return String.format( "NNN (%s)", profilArt.getLabel() );
  }

  protected String getDescription( final EwawiSta staIndex, final EwawiProfilePart basePart ) throws EwawiException
  {
    final StringBuilder description = new StringBuilder();

    final EwawiObjectart objectArt = basePart.getObjectArt( staIndex );
    if( objectArt != null && objectArt != EwawiObjectart._1100 )
    {
      final String objectArtText = String.format( "Objektart: %d, %s%n", objectArt.getKey(), objectArt.getLabel() );
      description.append( objectArtText );
    }

    final Short zusatz = basePart.getZusatz( staIndex );
    if( zusatz != null && zusatz != 0 )
    {
      final String zusatzText = String.format( "Zusatzkennzahl: %d%n", zusatz );
      description.append( zusatzText );
    }

    final Date validity = basePart.getValidity( staIndex );
    if( validity != null )
    {
      final DateFormat df = DateFormat.getDateInstance( DateFormat.MEDIUM );
      final String validityText = String.format( "G�ltigkeitsdatum: %s%n", df.format( validity ) );
      description.append( validityText );
    }

    final EwawiProfilart profilArt = basePart.getProfilArt( staIndex );
    if( profilArt != null )
    {
      final String profilArtText = String.format( "Profilart: %d, %s%n", profilArt.getKey(), profilArt.getLabel() );
      description.append( profilArtText );
    }

    final String comment = basePart.getComment( staIndex );
    if( comment != null && !comment.equals( "-" ) )
    {
      final String commentText = String.format( "Bemerkung: %s%n", comment );
      description.append( commentText );
    }

    return description.toString();
  }

  protected String getRiverId( final EwawiProfilePart basePart )
  {
    final Long gewKennzahl = basePart.getGewKennzahl();
    if( gewKennzahl == null )
      return "-1";

    return String.format( "%d", gewKennzahl );
  }

  protected String getRiverName( final EwawiImportData data, final GewShape gewShape, final EwawiProfilePart basePart ) throws DBaseException
  {
    if( gewShape == null )
      return "Undefiniert";

    final Long gewKennzahl = basePart.getGewKennzahl();
    if( gewKennzahl == null )
      return "Undefiniert";

    final String name = (String)gewShape.getValue( gewKennzahl, data.getRiverShapeData().getRiverNameField() );
    if( name == null )
      return "Undefiniert";

    return name;
  }

  protected GM_Curve getRiverGeometry( final EwawiImportData data, final GewShape gewShape, final EwawiProfilePart basePart ) throws DBaseException
  {
    if( gewShape == null )
      return null;

    final Long gewKennzahl = basePart.getGewKennzahl();
    if( gewKennzahl == null )
      return null;

    return (GM_Curve)gewShape.getValue( gewKennzahl, data.getRiverShapeData().getRiverGeometryField() );
  }
}