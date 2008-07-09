/*--------------- Kalypso-Header --------------------------------------------------------------------

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

 ---------------------------------------------------------------------------------------------------*/
package org.kalypso.ant;

import java.net.URL;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.jface.dialogs.ErrorDialog;
import org.eclipse.swt.widgets.Shell;
import org.kalypso.contribs.java.lang.reflect.ClassUtilities;
import org.kalypso.contribs.java.net.IUrlResolver;
import org.kalypso.contribs.java.util.logging.ILogger;
import org.kalypso.ogc.util.MergeObservationFeatureVisitor;
import org.kalypsodeegree.model.feature.FeatureVisitor;

/**
 * Dieser Ant-Task realisiert die �bernahme von Handeingaben. <br>
 * Alle Werte der Quell-Zeitreihe �berschreiben Werte der Zeil-Zeitreihe, wenn:
 * <ul>
 * <li>der Quellwert eine Handeingabe ist</li>
 * <li>der Zielwert das Warn-Flag hat</li>
 * <li>der Quellwert innerhalb des Zielzeitraums liegt</li>
 * <li></li>
 * </ul>
 * 
 * <code>
 *   <kalypso.mergeObservation gml="${project.url}/.model/observationConf/OmbrometerMapping.gml" featurePath="mappingMember" sourceContext="${calc.merge.url}" targetContext="${calc.url}" observationProperty="outObservationLink"/>
 * </code>
 * 
 * @author belger
 */
public class MergeObservationTask extends AbstractFeatureVisitorTask
{
  /**
   * Context, gegen den die Links der Quell-Zeitreihen aufgel�st werden
   */
  private URL m_sourceContext;

  /**
   * Name der Feature-Property, welche den Observation-Link enth�lt
   */
  private String m_observationProperty;

  public MergeObservationTask()
  {
    super( false );
  }

  /**
   * @see org.kalypso.ant.AbstractFeatureVisitorTask#createVisitor(URL, IUrlResolver, ILogger, IProgressMonitor)
   */
  protected final FeatureVisitor createVisitor( final URL context, final IUrlResolver resolver, final ILogger logger,
      final IProgressMonitor monitor )
  {
    return new MergeObservationFeatureVisitor( resolver, m_sourceContext, context, m_observationProperty, logger );
  }

  public final void setSourceContext( final URL context )
  {
    m_sourceContext = context;
  }

  public final void setObservationProperty( final String observationProperty )
  {
    m_observationProperty = observationProperty;
  }

  /**
   * @see org.kalypso.ant.AbstractFeatureVisitorTask#validateInput()
   */
  protected void validateInput()
  {
  // nothing to do
  }

  /**
   * @see org.kalypso.contribs.eclipse.jface.operation.IErrorHandler#handleError(org.eclipse.swt.widgets.Shell,
   *      org.eclipse.core.runtime.IStatus)
   */
  public void handleError( final Shell shell, final IStatus status )
  {
    ErrorDialog.openError( shell, ClassUtilities.getOnlyClassName( getClass() ), "Fehler beim Kopieren der Zeitreihen",
        status );
  }

}