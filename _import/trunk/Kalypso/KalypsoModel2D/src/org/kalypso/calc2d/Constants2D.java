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
/*
 * 
 * Created on 11.03.2005
 *
 */
package org.kalypso.calc2d;

/**
 * 
 * -----------------------------------------------------------------
 * 
 * @author katharina lupp <a href="mailto:k.lupp@web.de>Katharina Lupp </a>
 *  
 */

public interface Constants2D {

    public final String Boundary_ID = "Control";
    public final String MODELL_ID = "Modell";
    public final int CANCELED = 2;
    public final int ERROR = 4;
    public final int FINISHED = 1;
    public final int RUNNING = 0;
    public final int UNKNOWN = -1;
    public final int WAITING = 3;
    public final int WAITING_FOR_DATA = 5;

    public final String INPUT_DIR_NAME = "input";
    public final String OUTPUT_DIR_NAME = "output";
    public final String RESULT_DIR_NAME = "Results";
    public final String CALC_DIR_NAME = "calc";
//    public final String BASE_DIR_NAME = "C:/Programme/eclipse/workspace/Kalypso2d";
    public final String BASE_DIR_NAME = "C:\\Programme\\KalypsoServer\\data\\tmp\\TEST";
}