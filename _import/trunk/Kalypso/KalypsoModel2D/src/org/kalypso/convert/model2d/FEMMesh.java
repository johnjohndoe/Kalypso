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
 * Created on 12.01.2005
 *
 */
package org.kalypso.convert.model2d;

import java.io.IOException;
import java.io.PrintWriter;
import java.io.StringWriter;
import java.util.ArrayList;
import java.util.List;
import java.util.Locale;

import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree.model.geometry.GM_Position;
import org.kalypsodeegree.model.geometry.GM_Ring;
import org.kalypsodeegree.model.geometry.GM_Surface;
import org.kalypsodeegree.model.geometry.GM_SurfaceBoundary;
import org.kalypso.util.geom.GeometryHelper;

/**
 * This class is for organizing the parameters of the <MeshPolygon>for
 * converting the generated <GMLWorkspace>to the ascii file for the simulation
 * programm.
 * 
 * The parameters generated are: RK - id, roughness and color.
 * -----------------------------------------------------------------
 * 
 * @author katharina lupp <a href="mailto:k.lupp@web.de>Katharina Lupp </a>
 */
public class FEMMesh {

    private StringBuffer sbMesh = new StringBuffer();
    private ArrayList roughnessList = new ArrayList();
    private ArrayList colorList = new ArrayList();
    private ArrayList feList = new ArrayList();
    private ArrayList nodeList = new ArrayList();
    private ArrayList edgesList = new ArrayList();
    private ArrayList rough = new ArrayList();
    private int countEdge = 0;

    private Roughness roughness = new Roughness();
    private StringBuffer sbRoughness = new StringBuffer();
    private StringBuffer sbFE = new StringBuffer();
    private StringBuffer sbAR = new StringBuffer();
    private String roughOld = " ";
    private String roughInTS = " ";
    private String order = " ";
    private String dm = " ";
    private int z = 1;
    
    /**
     * Constructor
     */
    public FEMMesh() {

    }

    /**
     * Constructor
     * 
     * @param nodeList
     */
    public FEMMesh(ArrayList nodeList) {
        this.nodeList = nodeList;
    }

    /**
     * gets the properties of the <MeshPolygon>and separates it in the
     * parameters necessary for the ascii file of the simulation programm
     * 
     * @param ws
     * @param rootFeature
     */
    public void getMeshProperties(GMLWorkspace ws, Feature rootFeature) {
        Feature femCollectionFE = ws.resolveLink(rootFeature,
                "femCollectionMember");
        List meshFEs = (List) femCollectionFE.getProperty("meshMember");
        
        System.out.println("SIZE_FE " + meshFEs.size());
        for (int i = 0; i < meshFEs.size(); i++) {
            try {
	                Feature meshFE = (Feature) meshFEs.get(i);
	                //adds parameter RK and FE to <ArrayList>
	                this.addRKParameter(meshFE);
	                
	                int idFe = (int)Double.parseDouble(""+meshFE.getProperty("feId"));
	                this.addFEParameter(idFe, meshFE);
	
	                Object[] properties = meshFE.getProperties();
	                GM_Surface geomProperty = (GM_Surface) meshFE
	                        .getProperty("polygonProperty");
	
	                GM_SurfaceBoundary surfaceBoundary = geomProperty
	                        .getSurfaceBoundary();
	                GM_Ring exteriorRing = surfaceBoundary.getExteriorRing();
	                GM_Position[] positions = exteriorRing.getPositions();
	
	                this.getEdgesOfPolygon(idFe, positions);
            } catch (Exception ex) {
                System.out.println("no mesh feature could be generated ");
                ex.printStackTrace();
            }
        }
        
        this.setSB_AR();
        this.setSB_RK();
    }

    /**
     * gets the <Edges>of the particular <MeshPolygon>
     * 
     * @param idFE
     * @param position
     */
    private void getEdgesOfPolygon(int idFE, GM_Position[] position) {
        double[]a = new double[]{
            position[0].getX()-position[1].getX(),
	        position[0].getY()-position[1].getY(),
	        0d
        };
        double[]b = new double[]{
	        position[2].getX()-position[1].getX(),
	        position[2].getY()-position[1].getY(),
	        0d
        };
        int[] nodeIdOfPoly = this.getNodeListOfPolygon(position);
        
        Edge edge1 = new Edge((this.countEdge + 1), nodeIdOfPoly[1],nodeIdOfPoly[0]);
        organizeEdges(edge1, idFE, GeometryHelper.isAngleClockwise(a, b));
        
        Edge edge2 = new Edge((this.countEdge + 1), nodeIdOfPoly[2],nodeIdOfPoly[1]);
        organizeEdges(edge2, idFE, GeometryHelper.isAngleClockwise(a, b));
        
        Edge edge3 = new Edge((this.countEdge + 1), nodeIdOfPoly[3],nodeIdOfPoly[2]);
        organizeEdges(edge3, idFE, GeometryHelper.isAngleClockwise(a, b));

        Edge edge4 = new Edge();
        if (nodeIdOfPoly.length == 5) {
            edge4 = new Edge((this.countEdge + 1), nodeIdOfPoly[4],nodeIdOfPoly[3]);
            organizeEdges(edge4, idFE, GeometryHelper.isAngleClockwise(a, b));
        } else if (nodeIdOfPoly.length != 5 && nodeIdOfPoly.length != 4)
            System.out.println("MeshPolygon " + idFE
                    + " is not consisting of three or four edges");

    }

    /**
     * tests if this edge is already present in the <ArrayList>of edges.
     * 
     * @param edge
     * @param idFE
     */
    private void organizeEdges(Edge edge, int idFE, boolean isClockwise) {

        boolean existsClockwise = edge.existEdgeClockwise(this.edgesList, idFE);
        boolean existsNotClockwise = edge.existEdgeNotClockwise(this.edgesList, idFE);
        if (!existsClockwise && !existsNotClockwise && isClockwise) {
            edge.setFE1(idFE);
            this.edgesList.add(countEdge, edge);
            this.countEdge++;
        } else if(!existsClockwise && !existsNotClockwise && !isClockwise){
            edge.setFE2(idFE);
            this.edgesList.add(countEdge, edge);
            this.countEdge++;
            
        }
        else if(existsClockwise && isClockwise){
            for (int i = 0; i < this.edgesList.size(); i++) {
                Edge ed = (Edge) this.edgesList.get(i);
                if (edge.getP1() == ed.getP1() && edge.getP2() == ed.getP2()){
		            edge.setID(ed.getID());
                    edge.setFE1(idFE);
                    edge.setFE2(ed.getFE2());
                    this.edgesList.remove(ed);
		            this.edgesList.add(edge);
                    break;
                }
            }
            
            
        } else if(existsClockwise && !isClockwise){
            Edge edTmp = new Edge();
            for (int i = 0; i < this.edgesList.size(); i++) {
                Edge ed = (Edge) this.edgesList.get(i);
                if (edge.getP1() == ed.getP1() && edge.getP2() == ed.getP2()){
                    edge.setID(ed.getID());
		            edge.setFE2(idFE);
		            edge.setFE1(ed.getFE1());
		            this.edgesList.remove(ed);
		            this.edgesList.add(edge);
                    break;
                }
            }
        } 
        else if(existsNotClockwise && isClockwise){
            for (int i = 0; i < this.edgesList.size(); i++) {
                Edge ed = (Edge) this.edgesList.get(i);
                if(edge.getP2() == ed.getP1() && edge.getP1() == ed.getP2()){
                    edge.setID(ed.getID());
		            edge.setFE1(idFE);
		            edge.setFE2(ed.getFE1());
		            this.edgesList.remove(ed);
		            this.edgesList.add(edge);
                    break;
                }
            }
        } else if(existsNotClockwise && !isClockwise){
            for (int i = 0; i < this.edgesList.size(); i++) {
                Edge ed = (Edge) this.edgesList.get(i);
                if(edge.getP2() == ed.getP1() && edge.getP1() == ed.getP2()){
                    edge.setID(ed.getID());
                    edge.setFE2(idFE);
                    edge.setFE1(ed.getFE2());
                    this.edgesList.remove(ed);
		            this.edgesList.add(edge);
                    break;
                }
            }
            int posId = this.edgesList.indexOf(edge);
            boolean contains = this.edgesList.contains(edge);
        }
    }

    /**
     * sets an <ArrayList>with the nodeIds of the particular <MeshPolygon>
     * 
     * @param position
     * @return
     */
    private int[] getNodeListOfPolygon(GM_Position[] position) {
        int[] ids = new int[position.length];

        for (int i = 0; i < position.length; i++) {
            double x = position[i].getX();
            double y = position[i].getY();
            double z = position[i].getZ();

            ids[i] = this.getNodeId(x, y, z);
        }

        return ids;
    }

    /**
     * gets the id of the <Node>encountered in the particular <MeshPolygon>
     * 
     * @param x
     * @param y
     * @param z
     * @return
     */
    private int getNodeId(double x, double y, double z) {
        int idNode = 0;
        for (int i = 0; i < this.nodeList.size(); i++) {
            Node node = (Node) nodeList.get(i);
            if (node.getX() == x && node.getY() == y && node.getZ() == z) {
                idNode = node.getID();
                break;
            }
        }

        return idNode;
    }

    /**
     * adds the parameter of the finite element to an <ArrayList>
     * 
     * @param meshFE
     */
    private void addFEParameter(int k, Feature meshFE) {
        z++;
        if (meshFE.getProperty("roughnessOld") != null) {
            roughOld = (String) meshFE.getProperty("roughnessOld");
            ArrayList rough2 = this.getRough();
            roughOld = "" + (rough2.indexOf(roughOld) + 1);
            if (roughOld.equalsIgnoreCase("null"))
                roughOld = "" + 0;
        } else if (meshFE.getProperty("roughnessOld") == null)
            roughOld = "" + 0;

        if (meshFE.getProperty("roughnessInTimeWarp") != null) {
            roughInTS = (String) meshFE.getProperty("roughnessInTimeWarp");
            ArrayList rough3 = this.getRough();
            roughInTS = "" + (rough3.indexOf(roughOld) + 1);
            if (roughInTS.equalsIgnoreCase("null"))
                roughInTS = "" + 0;
        } else if (meshFE.getProperty("roughnessInTimeWarp") == null)
            roughInTS = "" + 0;

        if (meshFE.getProperty("orderOfProcessing") != null) {
            order = meshFE.getProperty("orderOfProcessing").toString();
        } else if (meshFE.getProperty("orderOfProcessing") == null)
            order = "" + 0;

        if (meshFE.getProperty("defaultMass") != null)
            dm = meshFE.getProperty("defaultMass").toString();
        String fe = k + "  " + roughOld + "  " + roughInTS + "  " + order
                + "  " + dm + "\n";
        feList.add(fe);
        
        setFEStringWriter(k, roughOld, roughInTS,order,dm);

        roughOld = " ";
        roughInTS = " ";
        order = " ";
        dm = " ";
    }
    
    /**
     * sets finite elements in fortran format
     * @param k
     * @param roughOld
     * @param roughInTS
     * @param order
     * @param dm
     * @return
     */
    private StringWriter setFEStringWriter(int k, String roughOld, String roughInTS,
            								String order, String dm){
        int ro = (int)Math.round( (Double.parseDouble(roughOld)) );
        int rts = (int)Math.round( (Double.parseDouble(roughInTS)) );
        int ord = (int)Math.round( (Double.parseDouble(order)) );
        double def_m = Double.parseDouble(dm);
        
        StringWriter stringWriter = new StringWriter();
        PrintWriter printWriter = new PrintWriter(stringWriter);
        String format="FE%10d%10d%10d%10d%15.7f";
        Object[] o = new Object[]{
                new Integer(k),
                new Integer(ro),
                new Integer(rts),
                new Integer(ord),
                new Double(def_m),
        };
        printWriter.printf(Locale.ENGLISH,format+"\n", o);

        this.sbFE.append(stringWriter.getBuffer());
        printWriter.close();
        try {
            stringWriter.close();
        } catch (IOException e) {
            e.printStackTrace();
        }
        
        return stringWriter;
    }

    /**
     * adds the parameters of roughness RK to <ArrayList>
     * 
     * @param meshFE
     */
    private void addRKParameter(Feature meshFE) {
        String rough = "0";
        String color = "0";
        
        if (meshFE.getProperty("roughness") != null){
            rough = (String) meshFE.getProperty("roughness");;
        }
        if (meshFE.getProperty("color")!=null){
            color = (String) meshFE.getProperty("color");
        }
        if(!rough.equalsIgnoreCase("0"))
            this.roughness.addRoughness(this.roughnessList, rough, color);
    }

    /**
     * gets the <StringBuffer>of the roughness parameter
     * 
     * @return
     */
    public StringBuffer getSB_RK() {
        return this.sbRoughness;
    }

    /**
     * sets the roughness in <StringBuffer>
     *  
     */
    private void setSB_RK() {
        ArrayList listRK = roughness.getRoughness();
        for (int i = 0; i < listRK.size(); i++) {
            StringWriter stringWriter = new StringWriter();
            PrintWriter printWriter = new PrintWriter(stringWriter);
            String s = ""+listRK.get(i);
            String[] strings = s.split("  ");
            s = strings[0];
            String s2 = strings[1];
            if (strings[1].equalsIgnoreCase("null"))s2= ""+0;
            
            String format="RK%5d%-40s%8s%10d%10d%5d%5d";
            Object[] o = new Object[]{
                    new Integer((i+1)),
                    s,
                    s2,
                    new Integer(0),
                    new Integer(0),
                    new Integer(0),
                    new Integer(0),
                    
            };
            printWriter.printf(Locale.ENGLISH,format+"\n", o);

            this.sbRoughness.append(stringWriter.getBuffer());
            printWriter.close();
            try {
                stringWriter.close();
            } catch (IOException e) {
                e.printStackTrace();
            }
        }
    }

    /**
     * gets the <ArrayList>with id and particular <Roughness>
     * 
     * @return
     */
    public ArrayList getRough() {
        ArrayList listRK = roughness.getRoughness();
        for (int i = 0; i < listRK.size(); i++) {
            String r = "" + listRK.get(i);
            String[] ro = r.split(" ");
            this.rough.add(i, ro[0]);
        }
        return this.rough;
    }

    /**
     * gets <StringBuffer>with the <Edge>informations
     * 
     * @return
     */
    public StringBuffer getSB_AR() {
        return this.sbAR;
    }

    /**
     * sets the line with the data for the <Edge>
     */
    private void setSB_AR() {
        ArrayList list = new ArrayList();
        list = this.edgesList;
        for (int i = 0; i < list.size(); i++) {
            Edge ed = (Edge) list.get(i);
            if (ed != null ) {
                StringWriter stringWriter = new StringWriter();
                PrintWriter printWriter = new PrintWriter(stringWriter);
                String format="AR%10d%10d%10d%10d%10d";
                Object[] o = new Object[]{
                        new Integer(ed.getID()),
                        new Integer(ed.getP1()),
                        new Integer(ed.getP2()),
                        new Integer(ed.getFE1()),
                        new Integer(ed.getFE2()),
                };
                printWriter.printf(Locale.ENGLISH,format+"\n", o);

                this.sbAR.append(stringWriter.getBuffer());
                printWriter.close();
                try {
                    stringWriter.close();
                } catch (IOException e) {
                    e.printStackTrace();
                }
            }
        }
    }

    /**
     * @return <StringBuffer>with FE parameters
     */
    public StringBuffer getSB_FE() {
        return this.sbFE;
    }

    /**
     * sets <StringBuffer>with FE parameters
     *  
     */
    private void setSB_FE() {
        for (int i = 0; i < feList.size(); i++) {
            String feStr = "FE  " + feList.get(i);
            this.sbFE.append(feStr);
        }
    }
}