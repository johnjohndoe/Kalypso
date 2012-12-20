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

package org.kalypso.ui.rrm.internal.hydrotops;

import java.io.File;
import java.nio.charset.Charset;
import java.util.HashMap;
import java.util.Map;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.kalypso.contribs.eclipse.jface.operation.ICoreRunnableWithProgress;
import org.kalypso.gml.ui.commands.importshape.ImportShapeWizardPage;
import org.kalypso.model.hydrology.binding.PolygonIntersectionHelper.ImportType;
import org.kalypso.model.hydrology.binding.SoilTypeCollection;
import org.kalypso.model.hydrology.binding.parameter.Parameter;
import org.kalypso.model.hydrology.binding.parameter.Soiltype;
import org.kalypso.model.hydrology.operation.hydrotope.PedologyImportOperation;
import org.kalypso.model.hydrology.operation.hydrotope.PedologyImportOperation.InputDescriptor;
import org.kalypso.model.hydrology.operation.hydrotope.PedologyShapeInputDescriptor;
import org.kalypso.ui.rrm.internal.KalypsoUIRRMPlugin;
import org.kalypso.ui.rrm.internal.i18n.Messages;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree.model.feature.IFeatureBindingCollection;

/**
 * @author Dejan Antanaskovic
 */
public class ImportPedologyWizard extends AbstractHydrotopeDataImportWizard
{
  private final static String PROPERTY_SOIL_TYPE = Messages.getString( "org.kalypso.ui.rrm.wizards.importPedologyData.ImportPedologyWizardPage.12" ); //$NON-NLS-1$

  public ImportPedologyWizard( )
  {
    setWindowTitle( Messages.getString( "org.kalypso.ui.rrm.wizards.importPedologyData.ImportPedologyWizardPage.0" ) ); //$NON-NLS-1$
  }

  @Override
  protected String[] getProperties( )
  {
    return new String[] { PROPERTY_SOIL_TYPE };
  }

  @Override
  protected String getDescription( )
  {
    return Messages.getString( "org.kalypso.ui.rrm.wizards.importPedologyData.ImportPedologyWizardPage.3" ); //$NON-NLS-1$
  }

  @Override
  protected ICoreRunnableWithProgress createImportOperation( final ImportShapeWizardPage wizardPage, final GMLWorkspace dataWorkspace, final Parameter parameter ) throws CoreException
  {
    final String soilTypeProperty = wizardPage.getProperty( PROPERTY_SOIL_TYPE );
    final File shapeFile = wizardPage.getShapeFile();
    final String crs = wizardPage.getSelectedCRS();
    final Charset charset = wizardPage.getSelectedCharset();

    final InputDescriptor inputDescriptor = new PedologyShapeInputDescriptor( shapeFile, soilTypeProperty, crs, charset );

    final SoilTypeCollection output = (SoilTypeCollection) dataWorkspace.getRootFeature();

    final Map<String, String> pedologyClasses = hashSoiltypeClasses( parameter );
    if( pedologyClasses.size() == 0 )
    {
      final String message = Messages.getString( "ImportPedologyWizard.0" ); //$NON-NLS-1$
      final IStatus status = new Status( IStatus.WARNING, KalypsoUIRRMPlugin.getID(), message );
      throw new CoreException( status );
    }

    return new PedologyImportOperation( inputDescriptor, output, pedologyClasses, ImportType.CLEAR_OUTPUT );
  }

  private Map<String, String> hashSoiltypeClasses( final Parameter parameter )
  {
    final Map<String, String> pedologyClasses = new HashMap<>();

    final IFeatureBindingCollection<Soiltype> soiltypes = parameter.getSoiltypes();
    for( final Soiltype soilType : soiltypes )
    {
      final String name = soilType.getName();
      final String id = soilType.getId();

      pedologyClasses.put( name, id );
    }

    return pedologyClasses;
  }
}