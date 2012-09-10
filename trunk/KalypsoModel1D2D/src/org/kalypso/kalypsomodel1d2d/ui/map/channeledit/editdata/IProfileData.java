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
package org.kalypso.kalypsomodel1d2d.ui.map.channeledit.editdata;

import org.deegree.model.spatialschema.GeometryException;
import org.kalypso.model.wspm.core.gml.IProfileFeature;
import org.kalypso.model.wspm.core.profil.IProfil;
import org.kalypso.transformation.transformer.GeoTransformerException;
import org.kalypsodeegree.model.geometry.GM_Envelope;
import org.kalypsodeegree.model.geometry.GM_Exception;

/**
 * represants a profile of the channel data.
 *
 * @author Gernot Belger
 */
public interface IProfileData
{
  IProfileFeature getFeature( );

  IProfil getProfilOrg( );

  ISegmentData getDownSegment( );

  ISegmentData getUpSegment( );

  // TODO: rename
  IProfil getProfIntersProfile( );

  GM_Envelope getSegmentMapExtent( String coordinatesSystem ) throws GeometryException;

  boolean isUserChaned( );

  void updateSegmentedProfile( IProfil newSegmentedProfile ) throws GM_Exception, GeoTransformerException;
}