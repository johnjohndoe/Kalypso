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
import org.kalypso.model.hydrology.binding.GeologyCollection;
import org.kalypso.model.hydrology.binding.PolygonIntersectionHelper.ImportType;
import org.kalypso.model.hydrology.binding.parameter.Parameter;
import org.kalypso.model.hydrology.operation.hydrotope.GeologyImportOperation;
import org.kalypso.model.hydrology.operation.hydrotope.GeologyImportOperation.InputDescriptor;
import org.kalypso.model.hydrology.operation.hydrotope.GeologyShapeInputDescriptor;
import org.kalypso.ui.rrm.internal.i18n.Messages;
import org.kalypsodeegree.model.feature.GMLWorkspace;

/**
 * @author Dejan Antanaskovic
 */
public class ImportGeologyWizard extends AbstractHydrotopeDataImportWizard
{
  private final static String PROPERTY_MAX_PERC_RATE = Messages.getString( "org.kalypso.ui.rrm.wizards.importGeologyData.ImportGeologyWizardPage.12" ); //$NON-NLS-1$

  private final static String PROPERTY_GW_FACTOR = Messages.getString( "org.kalypso.ui.rrm.wizards.importGeologyData.ImportGeologyWizardPage.14" ); //$NON-NLS-1$

  public ImportGeologyWizard( )
  {
    setWindowTitle( Messages.getString( "org.kalypso.ui.rrm.wizards.importGeologyDataImportGeologyWizard.0" ) ); //$NON-NLS-1$
  }

  @Override
  protected String[] getProperties( )
  {
    return new String[] { PROPERTY_MAX_PERC_RATE, PROPERTY_GW_FACTOR };
  }

  @Override
  protected String getDescription( )
  {
    return Messages.getString( "org.kalypso.ui.rrm.wizards.importGeologyData.ImportGeologyWizardPage.3" ); //$NON-NLS-1$
  }

  @Override
  protected ICoreRunnableWithProgress createImportOperation( final ImportShapeWizardPage wizardPage, final GMLWorkspace dataWorkspace, final Parameter parameter )
  {
    final String maxPerculationsRateProperty = wizardPage.getProperty( PROPERTY_MAX_PERC_RATE );
    final String gwFactorProperty = wizardPage.getProperty( PROPERTY_GW_FACTOR );

    final File shapeFile = wizardPage.getShapeFile();
    final String crs = wizardPage.getSelectedCRS();
    final Charset charset = wizardPage.getSelectedCharset();

    final InputDescriptor inputDescriptor = new GeologyShapeInputDescriptor( shapeFile, maxPerculationsRateProperty, gwFactorProperty, crs, charset );

    final GeologyCollection output = (GeologyCollection) dataWorkspace.getRootFeature();
    return new GeologyImportOperation( inputDescriptor, output, ImportType.CLEAR_OUTPUT );
  }
}