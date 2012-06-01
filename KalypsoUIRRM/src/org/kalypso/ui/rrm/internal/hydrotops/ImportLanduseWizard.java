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

import org.kalypso.contribs.eclipse.jface.operation.ICoreRunnableWithProgress;
import org.kalypso.gml.ui.commands.importshape.ImportShapeWizardPage;
import org.kalypso.model.hydrology.binding.LanduseCollection;
import org.kalypso.model.hydrology.binding.PolygonIntersectionHelper.ImportType;
import org.kalypso.model.hydrology.binding.parameter.Parameter;
import org.kalypso.model.hydrology.operation.hydrotope.DefaultLanduseClassDelegate;
import org.kalypso.model.hydrology.operation.hydrotope.LanduseImportOperation;
import org.kalypso.model.hydrology.operation.hydrotope.LanduseImportOperation.InputDescriptor;
import org.kalypso.model.hydrology.operation.hydrotope.LanduseShapeInputDescriptor;
import org.kalypso.ui.rrm.internal.i18n.Messages;
import org.kalypsodeegree.model.feature.GMLWorkspace;

/**
 * @author Dejan Antanaskovic
 */
public class ImportLanduseWizard extends AbstractHydrotopeDataImportWizard
{
  private final static String PROPERTY_LANDUSE = Messages.getString( "org.kalypso.ui.rrm.wizards.importLanduse.ImportLanduseWizardPage.12" ); //$NON-NLS-1$

  private final static String PROPERTY_SEALING_FACTOR = Messages.getString( "org.kalypso.ui.rrm.wizards.importLanduse.ImportLanduseWizardPage.14" ); //$NON-NLS-1$

  public ImportLanduseWizard( )
  {
    setWindowTitle( Messages.getString( "org.kalypso.ui.rrm.wizards.importLanduseImportLanduseWizard.0" ) ); //$NON-NLS-1$
  }

  @Override
  protected String[] getProperties( )
  {
    return new String[] { PROPERTY_LANDUSE, PROPERTY_SEALING_FACTOR };
  }

  @Override
  protected String getDescription( )
  {
    return Messages.getString( "org.kalypso.ui.rrm.wizards.importLanduse.ImportLanduseWizardPage.3" ); //$NON-NLS-1$
  }

  @Override
  protected ICoreRunnableWithProgress createImportOperation( final ImportShapeWizardPage wizardPage, final GMLWorkspace landuseWorkspace, final Parameter parameter )
  {
    final String landuseProperty = wizardPage.getProperty( PROPERTY_LANDUSE );
    final String sealingFactorProperty = wizardPage.getProperty( PROPERTY_SEALING_FACTOR );
    final File shapeFile = wizardPage.getShapeFile();
    final String crs = wizardPage.getSelectedCRS();
    final Charset charset = wizardPage.getSelectedCharset();

    final InputDescriptor inputDescriptor = new LanduseShapeInputDescriptor( shapeFile, landuseProperty, sealingFactorProperty, crs, charset );

    final LanduseCollection output = (LanduseCollection) landuseWorkspace.getRootFeature();
    final DefaultLanduseClassDelegate delegate = new DefaultLanduseClassDelegate( parameter.getWorkspace() );
    return new LanduseImportOperation( inputDescriptor, output, delegate, ImportType.CLEAR_OUTPUT );
  }
}