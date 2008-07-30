/*--------------- Kalypso-Header ------------------------------------------

 This file is part of kalypso.
 Copyright (C) 2004, 2005 by:

 Technical University Hamburg-Harburg (TUHH)
 Institute of River and coastal engineering
 Denickestr. 22
 21073 Hamburg, Germany
 http://www.tuhh.de/wb

 and

 Bjoernsen Consulting Engineers (BCE)
 Maria Trost 3
 56070 Koblenz, Germany
 http://www.bjoernsen.de

 This library is free software; you can redistribute it and/or
 modify it under the terms of the GNU Lesser General Public
 License as published by the Free Software Foundation; either
 version 2.1 of the License, or (at your option) any later version.

 This library is distributed in the hope that it will be useful,
 but WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 Lesser General Public License for more details.

 You should have received a copy of the GNU Lesser General Public
 License along with this library; if not, write to the Free Software
 Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

 Contact:

 E-Mail:
 belger@bjoernsen.de
 schlienger@bjoernsen.de
 v.doemming@tuhh.de

 --------------------------------------------------------------------------*/

package org.kalypso.metadoc.impl;

import java.lang.reflect.InvocationTargetException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Map.Entry;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.jface.resource.ImageDescriptor;
import org.kalypso.commons.arguments.Arguments;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.contribs.java.lang.ISupplier;
import org.kalypso.metadoc.IExporter;
import org.kalypso.metadoc.ui.ExportableTreeItem;

/**
 * This Exporter wraps multiple exporters (must all be of the same type) and exhibits them as one single exporter.
 * <p>
 * Argument: One or more exporters used for metadoc document exports. Many 'exporter' elements can be specified in the
 * arguments, they must be followed by some arbitrary string.
 * </p>
 * <p>
 * Each argument can contains sub-arguments which in turn are forwarded to the exporter object. This way, you can
 * specify exporter specific initialisation.
 * </p>
 * <p>
 * There must be at least the 'id' argument which contains the id of the exporter to use (for a list of valid exporter
 * ids, see the <code>org.kalypso.metadoc.exporter</code> extension point)
 * 
 * @author belger
 */
public class MultiExporter extends AbstractExporter
{
  private IExporter[] m_exporters;

  /**
   * @see org.kalypso.metadoc.impl.AbstractExporter#init(org.kalypso.contribs.java.lang.ISupplier)
   */
  @Override
  public void init( final ISupplier supplier ) throws CoreException
  {
    super.init( supplier );

    // read and create sub-exporters
    final Arguments arguments = (Arguments) getFromSupplier( "arguments" );

    final Collection<IStatus> stati = new ArrayList<IStatus>();
    final ISupplierCreator creator = new ISupplierCreator()
    {
      public ISupplier createSupplier( final Arguments args ) throws InvocationTargetException
      {
        return (ISupplier) supplier.supply( args );
      }
    };

    final Collection<IExporter> exporterList = createExporterFromArguments( stati, arguments, "exporter", creator );
    if( exporterList.isEmpty() )
      throw new CoreException( StatusUtilities.createErrorStatus( "Leerer Multi-Exporter nicht möglich." ) );
    m_exporters = exporterList.toArray( new IExporter[exporterList.size()] );

    final IExporter firstExporter = m_exporters[0];
    for( int i = 1; i < m_exporters.length; i++ )
    {
      final IExporter exporter = m_exporters[i];
      if( exporter.getClass() != firstExporter.getClass() )
        throw new CoreException( StatusUtilities.createErrorStatus( "Alle Exporter im Multi-Exporter müssen die gleiche Klasse haben." ) );
    }
  }

  /**
   * @see org.kalypso.metadoc.impl.AbstractExporter#createTreeItems(org.kalypso.metadoc.ui.ExportableTreeItem)
   */
  @Override
  protected ExportableTreeItem[] createTreeItems( final ExportableTreeItem parent ) throws CoreException
  {
    final ExportableTreeItem[] items = new ExportableTreeItem[m_exporters.length];
    for( int i = 0; i < m_exporters.length; i++ )
      items[i] = m_exporters[i].createTreeItem( parent );

    return items;
  }

  /**
   * Returns the image of the first of its exporters.
   * 
   * @see org.kalypso.metadoc.IExporter#getImageDescriptor()
   */
  @Override
  public ImageDescriptor getImageDescriptor( )
  {
    return m_exporters[0].getImageDescriptor();
  }

  /**
   * Creates exporters from a argument list of exporters.
   */
  public static Collection<IExporter> createExporterFromArguments( final Collection<IStatus> stati, final Arguments arguments, final String exporterKey, final ISupplierCreator supplierCreator )
  {
    final Collection<IExporter> exporters = new ArrayList<IExporter>();

    for( final Entry<String, Object> entry : arguments.entrySet() )
    {
      final String key = entry.getKey();
      if( key.startsWith( exporterKey ) )
      {
        try
        {
          final Arguments args = (Arguments) entry.getValue();
          final String exporterId = args.getProperty( "id" );
          if( exporterId == null )
            throw new CoreException( StatusUtilities.createWarningStatus( "Exporter ohne id konfiguriert: " + key ) );

          final IExporter exporter = MetadocExtensions.retrieveExporter( exporterId );
          // important: initialise the exporter
          exporter.init( supplierCreator.createSupplier( args ) );

          // if name provided, override name of exporter
          final String name = args.getProperty( "name" );
          if( name != null )
            exporter.setName( name );

          // if description provided, override description of exporter
          final String desc = args.getProperty( "description" );
          if( desc != null )
            exporter.setDescription( desc );

          exporters.add( exporter );
        }
        catch( final Exception e )
        {
          e.printStackTrace();

          stati.add( StatusUtilities.statusFromThrowable( e ) );
        }
      }
    }

    return exporters;
  }

  public static interface ISupplierCreator
  {
    public ISupplier createSupplier( final Arguments arguments ) throws InvocationTargetException;
  }

}
