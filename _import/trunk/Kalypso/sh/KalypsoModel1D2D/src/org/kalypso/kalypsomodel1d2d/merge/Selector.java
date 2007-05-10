/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 * 
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestraﬂe 22
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
package org.kalypso.kalypsomodel1d2d.merge;

import java.lang.reflect.InvocationTargetException;
import java.util.ArrayList;
import java.util.Collection;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.jface.operation.IRunnableWithProgress;
import org.kalypso.kalypsosimulationmodel.core.Assert;

/**
 * 
 * @author Patrice Congo
 */
public class Selector<T> implements IRunnableWithProgress
{
  /**
   * The test use to check the 
   */
  final ITest<T> test;
  
  /**
   * a collection of data to select from
   */
  final Collection<T> selectionSource;
  
  /**
   * cache the selected objects
   */
  final Collection<T> selected = new ArrayList<T>();

  /**
   * text describing the selection job
   */
  private String jobInfo;
  
  /**
   * Create a selector with the specific configuration
   * @param jobInfo info, description text about the selection jobs to be done
   * @param test {@link ITest} which provides the test
   * @param selectionSource the collection to select from
   * @throws IllegalArgumentException if any of the argument is null 
   *            or jobInfo is empty
   */
  public Selector( 
              final String jobInfo,
              final ITest<T> test, 
              final Collection<T> selectionSource )
  {
    Assert.throwIAEOnNullOrEmpty( jobInfo );
    Assert.throwIAEOnNullParam( test, "test" );
    Assert.throwIAEOnNullParam( selectionSource, "selectionSource" );
    
    this.test = test;
    this.selectionSource = selectionSource;
    this.jobInfo = jobInfo;
  }
  
  /**
   * @see org.eclipse.jface.operation.IRunnableWithProgress#run(org.eclipse.core.runtime.IProgressMonitor)
   */
  public void run( IProgressMonitor monitor ) throws InvocationTargetException, InterruptedException
  {
    selected.clear();
    
    if( monitor!=null )
    {
      monitor.beginTask( jobInfo, selectionSource.size() );
    }
    
    for( T candidate : selectionSource )
    {
      if( test.doesPass( candidate ) )
      {
        selected.add( candidate );
      }
      if( monitor != null )
      {
        if( monitor.isCanceled() )
        {
          selected.clear();
          throw new InterruptedException(
                  "Task Abgebrochen:"+jobInfo);
        }
        monitor.worked( 1 );
      }
    }
//    if( monitor != null )
//    {
//      monitor end
//    }
  }

  public Collection<T> getSelected( )
  {
    return selected;
  }
  
}
