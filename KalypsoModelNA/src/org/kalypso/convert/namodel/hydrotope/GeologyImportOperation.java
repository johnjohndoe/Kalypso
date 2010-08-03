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
package org.kalypso.convert.namodel.hydrotope;

import java.util.ArrayList;
import java.util.List;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.SubMonitor;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.contribs.eclipse.jface.operation.ICoreRunnableWithProgress;
import org.kalypso.contribs.eclipse.ui.progress.ProgressUtilities;
import org.kalypso.convert.namodel.schema.binding.Geology;
import org.kalypso.convert.namodel.schema.binding.GeologyCollection;
import org.kalypso.convert.namodel.schema.binding.PolygonIntersectionHelper.ImportType;
import org.kalypso.model.hydrology.internal.i18n.Messages;
import org.kalypsodeegree.model.feature.IFeatureBindingCollection;
import org.kalypsodeegree.model.geometry.GM_MultiSurface;

/**
 * Imports geology into a 'geology.gml' file from another gml-workspace (probably a shape-file).
 * 
 * @author Gernot Belger, Dejan Antanaskovic
 */
public class GeologyImportOperation implements ICoreRunnableWithProgress
{
  public static interface InputDescriptor
  {
    /** Number of elements contained in this descriptor. All other methods allow for indices in the range 0..size-1 */
    int size( ) throws CoreException;

    String getName( int index );

    String getDescription( int index );

    GM_MultiSurface getGeometry( int index ) throws CoreException;

    double getMaxPerkulationsRate( int index ) throws CoreException;

    double getGWFactor( int index ) throws CoreException;
  }

  private final GeologyCollection m_output;

  private final ImportType m_importType;

  private final InputDescriptor m_inputDescriptor;

  /**
   * @param output
   *          An (empty) list containing rrmgeology:geology features
   */
  public GeologyImportOperation( final InputDescriptor inputDescriptor, final GeologyCollection output, final ImportType importType )
  {
    m_inputDescriptor = inputDescriptor;
    m_output = output;
    m_importType = importType;
  }

  /**
   * @see org.kalypso.contribs.eclipse.jface.operation.ICoreRunnableWithProgress#execute(org.eclipse.core.runtime.IProgressMonitor)
   */
  @Override
  public IStatus execute( final IProgressMonitor monitor ) throws CoreException
  {
    final int size = m_inputDescriptor.size();
    final SubMonitor progess = SubMonitor.convert( monitor, Messages.getString("org.kalypso.convert.namodel.hydrotope.GeologyImportOperation.0"), size + 10 ); //$NON-NLS-1$

    final IFeatureBindingCollection<Geology> geologies = m_output.getGeologies();
    if( m_importType == ImportType.CLEAR_OUTPUT )
      geologies.clear();

    ProgressUtilities.worked( progess, 10 );

    final List<IStatus> log = new ArrayList<IStatus>();
    // traverse input workspace and import all single input geologies, if the geology class exists
    for( int i = 0; i < size; i++ )
    {
      try
      {
        final String label = m_inputDescriptor.getName( i );
        final GM_MultiSurface geometry = m_inputDescriptor.getGeometry( i );

        if( geometry == null )
        {
          final String message =  Messages.getString("org.kalypso.convert.namodel.hydrotope.GeologyImportOperation.1", label ); //$NON-NLS-1$
          log.add( StatusUtilities.createStatus( IStatus.WARNING, message, null ) );
        }

        final Geology geology = m_output.importGeology( label, geometry, m_importType, log );
        if( geology != null )
        {
          geology.setDescription( m_inputDescriptor.getDescription( i ) );
          geology.setMaxPerkulationsRate( m_inputDescriptor.getMaxPerkulationsRate( i ) );
          geology.setGWFactor( m_inputDescriptor.getGWFactor( i ) );
        }
      }
      catch( final CoreException e )
      {
        log.add( e.getStatus() );
      }

      ProgressUtilities.worked( progess, 1 );
    }

    return Status.OK_STATUS;
  }

}
