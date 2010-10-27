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
package org.kalypso.ui.rrm.wizards.conversion;

import java.io.File;

import org.eclipse.core.runtime.IProgressMonitor;
import org.kalypso.module.conversion.AbstractProjectConverter;
import org.kalypso.ui.rrm.wizards.conversion.from103to230.RrmProjectConverter103to230;

/**
 * {@link org.kalypso.module.conversion.IProjectConverter} implementation for KalypsoHydrology projects.
 * 
 * @author Gernot Belger
 */
public class RrmProjectConverter extends AbstractProjectConverter
{
  private final File m_sourceDir;

  private final File m_targetDir;

  public RrmProjectConverter( final File sourceDir, final File targetDir )
  {
    super( String.format( "Konvertierung von '%s'", sourceDir.getName() ) );

    m_sourceDir = sourceDir;
    m_targetDir = targetDir;
  }

  /**
   * @see org.kalypso.ui.rrm.wizards.conversion.AbstractLoggingOperation#doExecute(org.eclipse.core.runtime.IProgressMonitor)
   */
  @Override
  protected void doExecute( final IProgressMonitor monitor ) throws Exception
  {
    // FIXME: decide which converter to use, depending on version of source
    final RrmProjectConverter103to230 converter103to230 = new RrmProjectConverter103to230( m_sourceDir, m_targetDir );
    converter103to230.execute( monitor );
  }
}
