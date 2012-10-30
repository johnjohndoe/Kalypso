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
package org.kalypso.model.wspm.pdb.wspm;

import java.net.URI;

import org.eclipse.core.runtime.IStatus;
import org.kalypso.model.wspm.core.gml.IProfileFeature;
import org.kalypso.model.wspm.pdb.db.mapping.CrossSectionPartType;
import org.kalypso.model.wspm.pdb.db.mapping.State;
import org.kalypso.model.wspm.pdb.db.mapping.WaterBody;
import org.kalypso.model.wspm.pdb.db.utils.CrossSectionPartTypes;
import org.kalypso.model.wspm.pdb.gaf.GafCodes;
import org.kalypso.model.wspm.pdb.gaf.ICoefficients;
import org.kalypso.model.wspm.pdb.internal.wspm.ClassChecker;
import org.kalypso.model.wspm.tuhh.core.gml.TuhhReach;
import org.kalypso.transformation.transformer.GeoTransformerFactory;
import org.kalypso.transformation.transformer.IGeoTransformer;
import org.kalypsodeegree_impl.model.geometry.JTSAdapter;

import com.vividsolutions.jts.geom.GeometryFactory;
import com.vividsolutions.jts.geom.PrecisionModel;

/**
 * Data object for the {@link CheckinStateOperation}.
 *
 * @author Gernot Belger
 */
public class CheckinStateOperationData
{
  private final CrossSectionPartTypes m_partTypes;

  private final GafCodes m_gafCodes;

  private final ICoefficients m_coefficients;

  private final TuhhReach m_reach;

  private final IProfileFeature[] m_profiles;

  private final URI m_documentBase;

  private final IGeoTransformer m_transformer;

  private final GeometryFactory m_geometryFactory;

  private final ClassChecker m_classChecker;

  private final WaterBody m_waterBody;

  private final State m_state;

  private final String m_username;

  public CheckinStateOperationData( final CrossSectionPartTypes partTypes, final GafCodes gafCodes, final ICoefficients coefficients, final WaterBody waterBody, final State state, final TuhhReach reach, final IProfileFeature[] profiles, final String dbSrs, final URI documentBase, final String username )
  {
    m_partTypes = partTypes;
    m_gafCodes = gafCodes;
    m_coefficients = coefficients;
    m_waterBody = waterBody;
    m_state = state;
    m_reach = reach;
    m_profiles = profiles;
    m_documentBase = documentBase;
    m_username = username;

    m_classChecker = new ClassChecker( profiles );

    final int srid = JTSAdapter.toSrid( dbSrs );
    m_geometryFactory = new GeometryFactory( new PrecisionModel(), srid );

    m_transformer = GeoTransformerFactory.getGeoTransformer( dbSrs );
  }

  public GafCodes getGafCodes( )
  {
    return m_gafCodes;
  }

  public ICoefficients getCoefficients( )
  {
    return m_coefficients;
  }

  public TuhhReach getReach( )
  {
    return m_reach;
  }

  public IProfileFeature[] getProfiles( )
  {
    return m_profiles;
  }

  public URI getDocumentBase( )
  {
    return m_documentBase;
  }

  public CrossSectionPartType findPartType( final String partType )
  {
    if( m_partTypes == null )
      return null;

    final CrossSectionPartType type = m_partTypes.findPartType( partType );
    if( type == null )
      throw new IllegalArgumentException( String.format( "Unknown part type: %s", partType ) ); //$NON-NLS-1$

    return type;
  }

  public GeometryFactory getGeometryFactory( )
  {
    return m_geometryFactory;
  }

  public IGeoTransformer getTransformer( )
  {
    return m_transformer;
  }

  public IStatus checkClasses( )
  {
    return m_classChecker.execute();
  }

  public ClassChecker getClassChecker( )
  {
    return m_classChecker;
  }

  public WaterBody getWaterBody( )
  {
    return m_waterBody;
  }

  public State getState( )
  {
    return m_state;
  }

  public String getUsername( )
  {
    return m_username;
  }
}