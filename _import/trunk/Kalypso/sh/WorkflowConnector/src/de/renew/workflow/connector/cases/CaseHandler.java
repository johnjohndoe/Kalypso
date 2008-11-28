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

import java.util.regex.Pattern;

import org.eclipse.core.resources.IProject;

import de.renew.workflow.cases.Case;

/**
 * Wrapper interface for handling {@link Case} Objects
 * 
 * @author Dirk Kuch
 */
public class CaseHandler implements ICase
{
  private final Case m_caze;

  private final IProject m_project;

  public CaseHandler( final Case caze, final IProject project )
  {
    m_caze = caze;
    m_project = project;

    final String uri = CASE_BASE_URI.replaceFirst( Pattern.quote( "${casePath}" ), caze.getName() ); //$NON-NLS-1$ //$NON-NLS-2$
    setURI( uri );
  }

  /**
   * @see de.renew.workflow.connector.cases.ICase#getName()
   */
  @Override
  public String getName( )
  {
    return m_caze.getName();
  }

  /**
   * @see de.renew.workflow.connector.cases.ICase#getURI()
   */
  @Override
  public String getURI( )
  {

    return m_caze.getURI();
  }

  /**
   * @see de.renew.workflow.connector.cases.ICase#setName(java.lang.String)
   */
  @Override
  public void setName( final String name )
  {
    m_caze.setName( name );
  }

  /**
   * @see de.renew.workflow.connector.cases.ICase#setURI(java.lang.String)
   */
  @Override
  public void setURI( final String uri )
  {
    m_caze.setURI( uri );
  }

  /**
   * @see org.kalypso.afgui.scenarios.IScenario#getProject()
   */
  @Override
  public IProject getProject( )
  {
    return m_project;
  }

  /**
   * @see de.renew.workflow.connector.cases.ICase#getCase()
   */
  @Override
  public Case getCase( )
  {
    return m_caze;
  }

}
