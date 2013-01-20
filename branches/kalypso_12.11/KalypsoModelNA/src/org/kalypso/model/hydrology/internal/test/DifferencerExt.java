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
package org.kalypso.model.hydrology.internal.test;

import java.io.File;

import javax.xml.namespace.QName;

import org.eclipse.compare.structuremergeviewer.Differencer;
import org.eclipse.core.runtime.IStatus;
import org.kalypso.contribs.eclipse.compare.FileStructureComparator;
import org.kalypso.contribs.eclipse.core.runtime.StatusPrinter;
import org.kalypso.ogc.gml.compare.GmlComparator;
import org.kalypso.ogc.gml.serialize.GmlSerializer;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree_impl.model.feature.gmlxpath.GMLXPath;

/**
 * @author Gernot
 */
public class DifferencerExt extends Differencer
{
  private final String m_gmlFilename;

  private final GMLXPath[] m_listPaths;

  private final QName[] m_uniqueProperties;

  public DifferencerExt( final String gmlFilename, final GMLXPath[] listPaths, final QName[] uniqueProperties )
  {
    m_gmlFilename = gmlFilename;
    m_listPaths = listPaths;
    m_uniqueProperties = uniqueProperties;
  }

  @Override
  protected boolean contentsEqual( final Object input1, final Object input2 )
  {
    if( input1 instanceof FileStructureComparator )
    {
      final File file1 = ((FileStructureComparator)input1).getFile();
      final String name1 = file1.getName().toLowerCase();
      if( m_gmlFilename.equals( name1 ) )
      {
        final File file2 = ((FileStructureComparator)input2).getFile();
        return gmlEquals( file1, file2 );
      }
    }

    return super.contentsEqual( input1, input2 );
  }

  private boolean gmlEquals( final File file1, final File file2 )
  {
    try
    {
      final GMLWorkspace workspace1 = GmlSerializer.createGMLWorkspace( file1, null );
      final GMLWorkspace workspace2 = GmlSerializer.createGMLWorkspace( file2, null );

      final GmlComparator comparator = new GmlComparator( m_listPaths, m_uniqueProperties );
      final IStatus result = comparator.compare( workspace1, workspace2 );

      if( !result.isOK() )
      {
        final String dump = StatusPrinter.toString( result );
        System.err.println( dump );
      }

      workspace1.dispose();
      workspace2.dispose();

      return result.isOK();
    }
    catch( final Exception e )
    {
      e.printStackTrace();
      return false;
    }
  }
}
