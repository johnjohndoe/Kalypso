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
package org.kalypso.ui.model.wspm.tuhh.actions;

import java.net.URL;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.jface.action.IAction;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.ui.IActionDelegate;
import org.kalypso.contribs.eclipse.core.resources.ResourceUtilities;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.ogc.gml.selection.FeatureSelectionHelper;
import org.kalypso.ogc.gml.selection.IFeatureSelection;
import org.kalypso.simulation.core.simspec.Modeldata.ClearAfterCalc;
import org.kalypso.simulation.core.simspec.Modeldata.Input;
import org.kalypso.simulation.core.simspec.Modeldata.Output;
import org.kalypso.simulation.ui.calccase.ModelNature;
import org.kalypso.ui.model.wspm.abstraction.TuhhCalculation;
import org.kalypsodeegree.model.feature.Feature;

/**
 * @author belger
 */
public class CalcTuhhAction implements IActionDelegate
{
  private IFeatureSelection m_selection;

  /**
   * @see org.eclipse.ui.IActionDelegate#run(org.eclipse.jface.action.IAction)
   */
  public void run( final IAction action )
  {
    final Feature[] features = FeatureSelectionHelper.getFeatures( m_selection );
    for( final Feature feature : features )
    {
      final TuhhCalculation calculation = new TuhhCalculation( feature );

      final URL context = feature.getWorkspace().getContext();
      final IFile gmlFile = ResourceUtilities.findFileFromURL( context );
      if( gmlFile == null )
      {
        // context is not local, what shall we do?

        // ignore it for now
        continue;
      }

      final Job calcJob = new Job( "Berechnung von <" + calculation.getName() + ">" )
      {
        @Override
        protected IStatus run( final IProgressMonitor monitor )
        {
          try
          {
            final String calcxpath = "id('" + calculation.getFeature().getId() + "')";
            final String resultPath = "Ergebnisse/" + calculation.getName();

            final ModelNature nature = (ModelNature) gmlFile.getProject().getNature( ModelNature.ID );
            if( nature == null )
              return StatusUtilities.createWarningStatus( "Das Projekt ist kein Simulationsprojekt. D.h. Die Modelnature existiert nicht: " + ModelNature.ID );
            
            final Map<String, Object> properties = new HashMap<String, Object>();
            properties.put( "calc.xpath", calcxpath );
            properties.put( "result.path", resultPath );

            return nature.launchAnt( "Berechnung", "calc", properties, gmlFile.getParent(), monitor );
          }
          catch( final CoreException e )
          {
            return e.getStatus();
          }
        }
      };
      calcJob.schedule();
    }
  }

  protected void addInput( final List<Input> list, final String id, final String path, final boolean relative )
  {
    final Input input = ModelNature.OF_SPEC.createModeldataInput();
    input.setId( id );
    input.setPath( path );
    input.setRelativeToCalcCase( relative );
    list.add( input );
  }

  protected void addOutput( final List<Output> list, final String id, final String path, final boolean relative )
  {
    final Output output = ModelNature.OF_SPEC.createModeldataOutput();
    output.setId( id );
    output.setPath( path );
    output.setRelativeToCalcCase( relative );
    list.add( output );
  }

  protected void addClearAfterCalc( final List<ClearAfterCalc> list, final String path, final boolean relative )
  {
    final ClearAfterCalc clear = ModelNature.OF_SPEC.createModeldataClearAfterCalc();
    clear.setPath( path );
    clear.setRelativeToCalcCase( relative );
    list.add( clear );
  }

  /**
   * @see org.eclipse.ui.IActionDelegate#selectionChanged(org.eclipse.jface.action.IAction,
   *      org.eclipse.jface.viewers.ISelection)
   */
  public void selectionChanged( final IAction action, final ISelection selection )
  {
    m_selection = selection instanceof IFeatureSelection ? (IFeatureSelection) selection : null;

    // TODO: enable/disable depending on selection
    final boolean enable = selection != null && !selection.isEmpty();
    action.setEnabled( enable );
  }

}
