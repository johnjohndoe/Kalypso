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
package org.kalypso.model.wspm.tuhh.ui.export.sobek;

import java.io.IOException;
import java.math.BigDecimal;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.kalypso.model.wspm.core.profil.sobek.SobekModel;
import org.kalypso.model.wspm.core.profil.sobek.profiles.SobekNetworkD12Point;
import org.kalypso.model.wspm.core.profil.sobek.profiles.SobekProfile;
import org.kalypso.model.wspm.core.profil.sobek.struct.SobekStruct;
import org.kalypso.model.wspm.core.profil.sobek.struct.SobekStructDat;
import org.kalypso.model.wspm.tuhh.ui.KalypsoModelWspmTuhhUIPlugin;
import org.kalypso.shape.ShapeDataException;
import org.kalypso.shape.dbf.DBaseException;
import org.kalypso.shape.shp.SHPException;
import org.kalypsodeegree.model.geometry.GM_Point;
import org.kalypsodeegree_impl.model.geometry.GeometryFactory;

/**
 * @author Gernot Belger
 */
public class SobekProfileWriterOperation implements ISobekProfileExportOperation
{
  public static final String PROFILE_SHAPE_NAME = "profile"; //$NON-NLS-1$

  private static final String STRUCT_SHAPE_NAME = "struct"; //$NON-NLS-1$

  private final SobekExportInfo m_info;

  private final SobekModel m_sobekModel;

  public SobekProfileWriterOperation( final SobekExportInfo info, final SobekModel sobekModel )
  {
    m_info = info;
    m_sobekModel = sobekModel;
  }

  @Override
  public String getLabel( )
  {
    return "Write sobek files";
  }

  @Override
  public IStatus execute( final IProgressMonitor monitor ) throws CoreException
  {
    try
    {
      m_sobekModel.writeTo( m_info.getTargetDir() );

      writeProfilePointsShape();
      writeStructPointsShape();

      return Status.OK_STATUS;
    }
    catch( final IOException | DBaseException | ShapeDataException | SHPException e )
    {
      final IStatus status = new Status( IStatus.ERROR, KalypsoModelWspmTuhhUIPlugin.getID(), "Failed to write sobek model", e );
      throw new CoreException( status );
    }
  }

  private void writeProfilePointsShape( ) throws DBaseException, IOException, ShapeDataException, SHPException
  {
    final String srsName = m_info.getExportSrsName();
    final SobekShapePoint pointShape = new SobekShapePoint( srsName, m_info.getTargetDir(), PROFILE_SHAPE_NAME );

    final SobekProfile[] profiles = m_sobekModel.getProfiles();
    for( final SobekProfile profile : profiles )
    {
      final SobekNetworkD12Point networkPoint = profile.getNetworkPoint();
      if( networkPoint != null )
      {
        final BigDecimal px = networkPoint.getPX();
        final BigDecimal py = networkPoint.getPY();
        final String pointSRS = networkPoint.getSrsName();

        final GM_Point point = GeometryFactory.createGM_Point( px.doubleValue(), py.doubleValue(), pointSRS );

        final String id = networkPoint.getID();
        final String name = networkPoint.getName();

        pointShape.addEntry( point, id, name );
      }
    }

    pointShape.close();
  }

  private void writeStructPointsShape( ) throws IOException, DBaseException, ShapeDataException, SHPException
  {
    final String srsName = m_info.getExportSrsName();
    final SobekShapePoint pointShape = new SobekShapePoint( srsName, m_info.getTargetDir(), STRUCT_SHAPE_NAME );

    final SobekStruct[] structs = m_sobekModel.getStructs();
    for( final SobekStruct struct : structs )
    {
      final GM_Point networkPoint = struct.getLocation();
      final SobekStructDat dat = struct.getDat();
      if( networkPoint != null && dat != null )
      {
        final String id = dat.getID();
        final String name = dat.getName();

        pointShape.addEntry( networkPoint, id, name );
      }
    }

    pointShape.close();
  }
}