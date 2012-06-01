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
package org.kalypso.ui.rrm.internal.conversion.to12_02;

import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.IOException;
import java.net.MalformedURLException;
import java.net.URL;
import java.util.List;

import javax.xml.bind.JAXBContext;
import javax.xml.bind.JAXBException;

import org.apache.commons.io.IOUtils;
import org.kalypso.afgui.scenarios.ObjectFactory;
import org.kalypso.afgui.scenarios.Scenario;
import org.kalypso.afgui.scenarios.ScenarioList;
import org.kalypso.commons.bind.JaxbUtilities;
import org.kalypso.model.hydrology.project.INaProjectConstants;

import de.renew.workflow.cases.Case;
import de.renew.workflow.cases.CaseList;

/**
 * Grants access to some global data to the calc case converter.
 * 
 * @author Gernot Belger
 * @author Holger Albert
 */
public class GlobalConversionData
{
  private final static JAXBContext JAXB_CONTEXT = JaxbUtilities.createQuiet( org.kalypso.afgui.scenarios.ObjectFactory.class, de.renew.workflow.cases.ObjectFactory.class );

  private final static ObjectFactory OBJECT_FACTORY = new org.kalypso.afgui.scenarios.ObjectFactory();

  /**
   * The directory of the project to be imported.
   */
  private final File m_sourceDir;

  /**
   * The directory of the new project.
   */
  private final File m_targetDir;

  /**
   * The choosen exe.
   */
  private final String m_chosenExe;

  /**
   * The timeseries index.
   */
  private final TimeseriesIndex m_timeseriesIndex;

  private final File m_baseScenarioDir;

  /**
   * The case list.
   */
  private CaseList m_caseList;

  /**
   * The constructor.
   * 
   * @param sourceDir
   *          The directory of the project to be imported.
   * @param targetDir
   *          The directory of the new project.
   * @param chosenExe
   *          The choosen exe.
   * @param timeseriesIndex
   *          The timeseries index.
   */
  public GlobalConversionData( final File sourceDir, final File targetDir, final String chosenExe, final TimeseriesIndex timeseriesIndex )
  {
    m_sourceDir = sourceDir;
    m_targetDir = targetDir;
    m_chosenExe = chosenExe;
    m_timeseriesIndex = timeseriesIndex;
    m_baseScenarioDir = new File( targetDir, INaProjectConstants.FOLDER_BASIS );
    m_caseList = null;
  }

  /**
   * This function returns the directory of the project to be imported.
   * 
   * @return The directory of the project to be imported.
   */
  public File getSourceDir( )
  {
    return m_sourceDir;
  }

  /**
   * This function returns the directory of the new project.
   * 
   * @return The directory of the new project.
   */
  public File getTargetDir( )
  {
    return m_targetDir;
  }

  /**
   * This function returns the choosen exe.
   * 
   * @return The choosen exe.
   */
  public String getChosenExe( )
  {
    return m_chosenExe;
  }

  /**
   * This function returns the timeseries index.
   * 
   * @return The timeseries index.
   */
  public TimeseriesIndex getTimeseriesIndex( )
  {
    return m_timeseriesIndex;
  }

  /**
   * This function returns the directory of the base scenario.
   * 
   * @return The directory of the base scenario.
   */
  public File getBaseScenarioDir( )
  {
    return m_baseScenarioDir;
  }

  public void updateCasesFile( final File scenarioDir ) throws JAXBException, IOException
  {
    /* Load the existing cases.xml. */
    if( m_caseList == null )
      m_caseList = loadCaseList();

    /* Update the cases.xml. */
    final List<Case> allCases = m_caseList.getCases();
    for( final Case oneCase : allCases )
    {
      if( oneCase.getName().equals( "Basis" ) )
      {
        updateCase( (Scenario) oneCase, scenarioDir );
        break;
      }
    }

    /* Save the cases.xml. */
    saveCaseList();
  }

  private CaseList loadCaseList( ) throws MalformedURLException, JAXBException
  {
    final File casesXml = new File( m_targetDir, ".metadata/cases.xml" );
    final URL url = casesXml.toURI().toURL();
    return (CaseList) JAXB_CONTEXT.createUnmarshaller().unmarshal( url );
  }

  private void updateCase( final Scenario oneCase, final File scenarioDir )
  {
    final Scenario scenario = OBJECT_FACTORY.createScenario();
    scenario.setName( scenarioDir.getName() );
    scenario.setParentScenario( oneCase );
    scenario.setURI( String.format( "%s/%s/%s", oneCase.getURI(), ScenariosExclusionFileFilter.SCENARIOS_FOLDER, scenarioDir.getName() ) );

    final ScenarioList derivedScenarios = oneCase.getDerivedScenarios();
    derivedScenarios.getScenarios().add( scenario );
  }

  private void saveCaseList( ) throws JAXBException, IOException
  {
    ByteArrayOutputStream bos = null;

    try
    {
      bos = new ByteArrayOutputStream();
      JAXB_CONTEXT.createMarshaller().marshal( m_caseList, bos );
      bos.close();
    }
    finally
    {
      IOUtils.closeQuietly( bos );
    }
  }
}