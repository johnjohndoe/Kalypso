/*--------------- Kalypso-Header --------------------------------------------------------------------

 This file is part of ekalypso:
 Internet based elearning for complex simulation applications
 ("Internet basiertes E-Learning an komplexen Simulationsprogrammen [de]")

 The implementation is realised by: 
 Technical University Hamburg-Harburg (TUHH)
 Institute of River and coastal engineering
 Denickestr. 22
 21073 Hamburg, Germany
 http://www.tuhh.de/wb

 The project is sponsored and supported by:  
 local authority of education and research, 
 E-Learning Consortium Hamburg (ELCH) and
 Multimedia Kontor Hamburg.
 
 As this implementation depends on third party open source 
 java code it is consequently also licenced as open source
 in the hope that it will be useful specially (but not exclusively)
 to other e-learning projects that may extend or use this project.
 
 Copyright (C) 2004, 2005 by:
 Technical University Hamburg-Harburg (TUHH)
 Institute of River and coastal engineering
 Denickestr. 22
 21073 Hamburg, Germany
 http://www.tuhh.de/wb

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
 katharina.lupp@tuhh.de
 
 ---------------------------------------------------------------------------------------------------*/
/**
 * Created on 07.03.2005
 */

package org.kalypso.calc2d;

import java.io.File;

import org.kalypso.services.calculation.job.ICalcJob;
import org.kalypso.services.calculation.service.CalcJobDataBean;
import org.kalypso.services.calculation.service.CalcJobServiceException;

/**
 * @author katharina lupp <a href="mailto:k.lupp@web.de>Katharina Lupp </a>
 *  
 */
public class CalcJob2dModel implements ICalcJob {

    private ICalcJob calcJob = null;

  
    /**
     * @see org.kalypso.services.calculation.job.ICalcJob#run(java.io.File,
     *      org.kalypso.services.calculation.service.CalcJobDataBean[])
     */
    public void run(File basedir, CalcJobDataBean[] input)
            throws CalcJobServiceException {
      System.out.println("CalcJob2dModel.run");
        calcJob = new CalcJob2d();
        if (calcJob != null)
            calcJob.run(basedir, input);

    }

    /**
     * @see org.kalypso.services.calculation.job.ICalcJob#disposeJob()
     */
    public void disposeJob() {
        if (calcJob != null)
            calcJob.disposeJob();

    }

    /**
     * @see org.kalypso.services.calculation.job.ICalcJob#cancel()
     */
    public void cancel() {
        if (calcJob != null)
            calcJob.cancel();

    }

    /**
     * @see org.kalypso.services.calculation.job.ICalcJob#isCanceled()
     */
    public boolean isCanceled() {
        if (calcJob != null)
            return calcJob.isCanceled();

        return false;
    }

    /**
     * @see org.kalypso.services.calculation.job.ICalcJob#getProgress()
     */
    public int getProgress() {

        if (calcJob != null)
            return calcJob.getProgress();

        return 0;
    }

    /**
     * @see org.kalypso.services.calculation.job.ICalcJob#getMessage()
     */
    public String getMessage() {

        if (calcJob != null)
            return calcJob.getMessage();

        return "";
    }

    /**
     * @see org.kalypso.services.calculation.job.ICalcJob#getResults()
     */
    public CalcJobDataBean[] getResults() {

        if (calcJob != null)
            return calcJob.getResults();

        return null;
    }

}