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
package org.kalypso.model.wspm.pdb.ui.internal.tin.imports;

import java.io.File;

import org.eclipse.core.resources.IContainer;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.kalypso.commons.databinding.validation.TypedValidator;
import org.kalypso.gml.ui.coverage.imports.AbstractGridCoverageImporter;
import org.kalypso.gml.ui.coverage.imports.CoverageFormats;
import org.kalypso.gml.ui.coverage.imports.ICoverageImporter;
import org.kalypso.model.wspm.pdb.db.mapping.DhmIndex;
import org.kalypso.model.wspm.pdb.ui.internal.WspmPdbUiPlugin;

/**
 * @author Holger Albert
 */
public class ExistsValidator extends TypedValidator<DhmIndex>
{
  protected final PdbImportConnectionChooserData m_settingsData;

  private final IContainer m_dataContainer;

  public ExistsValidator( final Class<DhmIndex> type, final int severity, final String message, final PdbImportConnectionChooserData settingsData, final IContainer dataContainer )
  {
    super( type, severity, message );

    m_settingsData = settingsData;
    m_dataContainer = dataContainer;
  }

  @Override
  protected IStatus doValidate( final DhmIndex value ) throws CoreException
  {
    final String filename = value.getFilename();
    final IPath demServerPath = m_settingsData.getDemServerPath();
    final IPath filePath = demServerPath.append( filename );
    final File file = filePath.toFile();

    final ICoverageImporter importer = CoverageFormats.findImporter( file );
    if( importer == null )
      return new Status( IStatus.ERROR, WspmPdbUiPlugin.PLUGIN_ID, "DHM Format wird nicht unterstützt." );

    final File targetDir = m_dataContainer.getLocation().toFile();
    final String suffix = importer.getTargetExtension();
    final File targetFile = AbstractGridCoverageImporter.createTargetFile( file, targetDir, suffix );
    if( targetFile.exists() )
      fail();

    return new Status( IStatus.OK, WspmPdbUiPlugin.PLUGIN_ID, "OK" ); //$NON-NLS-1$
  }
}