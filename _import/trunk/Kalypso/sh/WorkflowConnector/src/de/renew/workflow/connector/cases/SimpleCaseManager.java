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
package de.renew.workflow.connector.cases;

import java.io.UnsupportedEncodingException;
import java.net.URLEncoder;
import java.util.List;
import java.util.regex.Pattern;

import javax.xml.bind.JAXBContext;
import javax.xml.bind.JAXBException;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;

import de.renew.workflow.cases.Case;

/**
 * @author Stefan Kurzbach
 * 
 */
public class SimpleCaseManager extends AbstractCaseManager<Case> implements ICaseManager<Case>
{

  public SimpleCaseManager( final IProject project ) throws CoreException
  {
    super( project, createJAXBContext() );
  }

  private static JAXBContext createJAXBContext( )
  {
    try
    {
      return JAXBContext.newInstance( de.renew.workflow.cases.ObjectFactory.class );
    }
    catch( final JAXBException e )
    {
      e.printStackTrace();
      return null;
    }
  }

  /**
   * @see de.renew.workflow.connector.context.AbstractCaseManager#createCase(java.lang.String)
   */
  @Override
  public Case createCase( final String name )
  {
    final Case newCase = new de.renew.workflow.cases.ObjectFactory().createCase();
    try
    {
      final String projectName = URLEncoder.encode( m_project.getName(), "UTF-8" );
      final String uri = CASE_BASE_URI.replaceFirst( Pattern.quote( "${project}" ), projectName ).replaceFirst( Pattern.quote( "${casePath}" ), name );
      newCase.setURI( uri );
    }
    catch( UnsupportedEncodingException e )
    {
      e.printStackTrace();
    }
    newCase.setName( name );
    internalAddCase( newCase );
    persist( null );
    fireCaseAdded( newCase );
    return newCase;
  }

  /**
   * @see de.renew.workflow.connector.context.AbstractCaseManager#removeCase(de.renew.workflow.cases.Case,
   *      org.eclipse.core.runtime.IProgressMonitor)
   */
  @Override
  public void removeCase( final Case caze, final IProgressMonitor monitor )
  {
    internalRemoveCase( caze );
    persist( null );
    fireCaseRemoved( caze );
  }

  /**
   * @see de.renew.workflow.connector.context.ICaseManager#getCase(java.lang.String)
   */
  public Case getCase( final String uri )
  {
    for( final Case caze : getCases() )
    {
      if( caze.getURI().equals( uri ) )
      {
        return caze;
      }
    }
    return null;
  }

  /**
   * @see de.renew.workflow.connector.context.ICaseManager#getCases()
   */
  public List<Case> getCases( )
  {
    return internalGetCases();
  }

}
