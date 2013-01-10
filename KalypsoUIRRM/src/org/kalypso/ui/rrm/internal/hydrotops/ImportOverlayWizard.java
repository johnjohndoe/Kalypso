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

import org.kalypso.contribs.eclipse.jface.operation.ICoreRunnableWithProgress;
import org.kalypso.gml.ui.commands.importshape.ImportShapeWizardPage;
import org.kalypso.model.hydrology.binding.OverlayCollection;
import org.kalypso.model.hydrology.binding.PolygonIntersectionHelper.ImportType;
import org.kalypso.model.hydrology.binding.parameter.DRWBMDefinition;
import org.kalypso.model.hydrology.binding.parameter.Parameter;
import org.kalypso.model.hydrology.operation.hydrotope.OverlayImportOperation;
import org.kalypso.model.hydrology.operation.hydrotope.OverlayImportOperation.InputDescriptor;
import org.kalypso.model.hydrology.operation.hydrotope.OverlayShapeInputDescriptor;
import org.kalypso.ui.rrm.internal.i18n.Messages;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree.model.feature.IFeatureBindingCollection;

/**
 * @author Dejan Antanaskovic
 */
public class ImportOverlayWizard extends AbstractHydrotopeDataImportWizard
{
  private final static String PROPERTY_DRWBM_DEFINITION = "DRWBM Definition";

  private final static String PROPERTY_DESCRIPTION = "Description";

  public ImportOverlayWizard( )
  {
    setWindowTitle( Messages.getString( "org.kalypso.ui.rrm.wizards.ImportOverlayWizard.0" ) ); //$NON-NLS-1$
  }

  @Override
  protected String[] getProperties( )
  {
    return new String[] { PROPERTY_DESCRIPTION, PROPERTY_DRWBM_DEFINITION };
  }

  @Override
  protected String[] getOptionalProperties( )
  {
    // all properties are optional
    return getProperties();
  }

  @Override
  protected String getDescription( )
  {
    return Messages.getString( "org.kalypso.ui.rrm.wizards.ImportOverlayWizard.1" ); //$NON-NLS-1$
  }

  @Override
  protected ICoreRunnableWithProgress createImportOperation( final ImportShapeWizardPage wizardPage, final GMLWorkspace overlayWorkspace, final Parameter parameter )
  {
    final String drwbmLabel = wizardPage.getProperty( PROPERTY_DESCRIPTION );
    final String drwbpmProperty = wizardPage.getProperty( PROPERTY_DRWBM_DEFINITION );

    final File shapeFile = wizardPage.getShapeFile();
    final String crs = wizardPage.getSelectedCRS();
    final Charset charset = wizardPage.getSelectedCharset();

    final InputDescriptor inputDescriptor = new OverlayShapeInputDescriptor( shapeFile, drwbmLabel, drwbpmProperty, crs, charset );

    final Map<String, String> drwbmClasses = hashDrwbmDefinitions( parameter );
    // REMARK: no check for empty drwbm types: overlay can be used without soiltypes

    final OverlayCollection output = (OverlayCollection)overlayWorkspace.getRootFeature();

    return new OverlayImportOperation( inputDescriptor, output, drwbmClasses, ImportType.CLEAR_OUTPUT );
  }

  private Map<String, String> hashDrwbmDefinitions( final Parameter parameter )
  {
    final Map<String, String> definitions = new HashMap<>();

    final IFeatureBindingCollection<DRWBMDefinition> drwbmDefinitions = parameter.getDRWBMDefinitions();
    for( final DRWBMDefinition definition : drwbmDefinitions )
    {
      final String name = definition.getName();
      final String id = definition.getId();

      definitions.put( name, id );
    }

    return definitions;
  }
}