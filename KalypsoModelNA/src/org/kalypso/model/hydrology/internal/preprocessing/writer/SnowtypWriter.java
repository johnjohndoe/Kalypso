/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 * 
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestraße 22
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
package org.kalypso.model.hydrology.internal.preprocessing.writer;

import java.io.PrintWriter;

import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.kalypso.model.hydrology.binding.parameter.Parameter;
import org.kalypso.model.hydrology.binding.parameter.Snow;
import org.kalypso.model.hydrology.internal.i18n.Messages;
import org.kalypsodeegree.model.feature.IFeatureBindingCollection;

/**
 * @author huebsch
 */
class SnowtypWriter extends AbstractCoreFileWriter
{
  private final Parameter m_parameter;

  public SnowtypWriter( final Parameter parameter )
  {
    m_parameter = parameter;
  }

  @Override
  protected IStatus writeContent( final PrintWriter writer )
  {
    writer.append( Messages.getString( "org.kalypso.convert.namodel.manager.SchneeManager.0" ) ); //$NON-NLS-1$
    writer.append( "\n/                     wwo wwmax snotem snorad h0\n" ); //$NON-NLS-1$
    writer.append( "/                      *    *     *      *    *\n" ); //$NON-NLS-1$

    final IFeatureBindingCollection<Snow> snowMembers = m_parameter.getSnow();
    for( final Snow snow : snowMembers )
    {
      // TODO: nur die schreiben, die auch in Gebietsdatei vorkommen
      writeSnow( writer, snow );
    }

    return Status.OK_STATUS;
  }

  private void writeSnow( final PrintWriter snowBuffer, final Snow snow )
  {
    final String name = snow.getName();
    final double xwwo = snow.getXwwo();
    final double xwwmax = snow.getXwwmax();
    final double xsnotem = snow.getXsnotem();
    final double xsnorad = snow.getXsnorad();
    final double xh0 = snow.getXh0();

    // (name,a20)(xwwo,*)_(xwwmax,*)_(xsnotem,*)_(xsnorad,*)_(xh0,*)
    snowBuffer.format( "%-20s%s %s %s %s %s\n", name, xwwo, xwwmax, xsnotem, xsnorad, xh0 ); //$NON-NLS-1$
  }

}
