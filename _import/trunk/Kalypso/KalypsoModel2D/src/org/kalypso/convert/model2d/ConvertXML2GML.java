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
 * Created on 02.09.2004
 */
package org.kalypso.convert.model2d;

import java.io.File;
import java.io.FileOutputStream;
import java.io.OutputStreamWriter;
import java.util.List;
import java.util.Vector;

import javax.xml.bind.Unmarshaller;
import javax.xml.transform.OutputKeys;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.dom.DOMSource;
import javax.xml.transform.stream.StreamResult;

import org.kalypso.convert.simulation2d.FpParams;
import org.kalypso.convert.simulation2d.ObjectFactory;
import org.kalypso.convert.simulation2d.Simulation2D;
import org.kalypso.convert.simulation2d.impl.FpParamsImpl;
import org.kalypso.convert.simulation2d.impl.Simulation2DTypeImpl;
import org.kalypso.convert.util.XMLHelper;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.NodeList;

/**
 * This class converts a xml file to gml using java & xml binding (jaxb)
 * generated classes.
 * ---------------------------------------------------------------------
 * 
 * @author Katharina Lupp <a href="mailto:k.lupp@web.de>Katharina Lupp </a>
 *         Created on 02.09.2004
 */
public class ConvertXML2GML {
    private static String comment;
    private static String rasterFileName;
    private static float time;
    private static String iteration;

    //FP
    private static double[] coordX = new double[100000];
    private static double[] coordY = new double[100000];
    private static double[] coordZ = new double[100000];

    //AR
    private static Vector outPolygons = new Vector();
    private static int[] p1 = new int[100000];
    private static int[] p2 = new int[100000];
    private static int[] e_left = new int[100000];
    private static int[] e_right = new int[100000];
    private static int[] e_midSideNode = new int[100000];
    private static String[][] ar = new String[100000][6];

    //FE
    private static int[] feId = new int[100000];
    private static int[] ro = new int[100000];
    private static int[] rtw = new int[100000];
    private static int[] oop = new int[100000];
    private static float[] dm = new float[100000];

    //RK
    private static String[] roughness = new String[100000];
    private static String[] color = new String[100000];
    private static int[] param1 = new int[100000];
    private static int[] param2 = new int[100000];
    private static int[] param3 = new int[100000];
    private static int[] param4 = new int[100000];

    //VA
    private static float[] xVelocity = new float[100000];
    private static float[] yVelocity = new float[100000];
    private static float[] riverDepth = new float[100000];
    private static float[] waterLevel = new float[100000];

    //GA
    private static float[] timeGradient1 = new float[100000];
    private static float[] timeGradient2 = new float[100000];
    private static float[] timeGradient3 = new float[100000];

    //VO
    private static float[] degreeOfFreedomOfOldTimeGradient1 = new float[100000];
    private static float[] degreeOfFreedomOfOldTimeGradient2 = new float[100000];
    private static float[] degreeOfFreedomOfOldTimeGradient3 = new float[100000];

    //GO
    private static float[] timeGradientOfFormerTimeWarps1 = new float[100000];
    private static float[] timeGradientOfFormerTimeWarps2 = new float[100000];
    private static float[] timeGradientOfFormerTimeWarps3 = new float[100000];

    //ZU
    private static float[] info1 = new float[100000];
    private static float[] info2 = new float[100000];
    private static float[] info3 = new float[100000];
    private static float[] info4 = new float[100000];
    private static float[] info5 = new float[100000];
    private static float[] info6 = new float[100000];

    //namespaces
    private static String NS = "http://elbe.wb.tu-harburg.de/2dModel";
    private static String GML_NS = "http://www.opengis.net/gml";
    private static String XLINK_NS = "http://www.w3.org/1999/xlink";
    private static String XSI_NS = "http://www.w3.org/2001/XMLSchema-instance";
    private static String XSI_schemaLocation_NS = "http://elbe.wb.tu-harburg.de/2dModel project:/.model/schema/2dgml.xsd";
    private static int sizeFP;

    private static int sizeElements;
    private static int removeElem;

    //for testing
    private static float m_xoffset = 3500000;
    private static float m_yoffset = 5900000;

    /**
     * gets the comment of the asci file
     */
    private String getComment() {
        return comment;
    }

    /**
     * gets the raster file name
     * 
     * @return
     */
    private String getRasterFileName() {
        return rasterFileName;
    }

    /**
     * set the commentString
     */
    private static void setComment(Simulation2D sim) {
        if (sim.getComment() != null)
            comment = sim.getComment().getCom();
    }

    /**
     * sets the raster file url
     * 
     * @param sim
     */
    private static void setRasterFile(Simulation2D sim) {
        if (sim.getRasterFile() != null)
            rasterFileName = sim.getRasterFile().getName();
    }

    /**
     * sets the time value
     * 
     * @param sim
     */
    private static void setTime(Simulation2D sim) {
        if (sim.getTime() != null)
            time = sim.getTime().getTimeParam();
    }

    /**
     * sets the iteration value
     * 
     * @param sim
     */
    private static void setIteration(Simulation2D sim) {
        if (sim.getTime() != null)
            iteration = sim.getTime().getIteration();
    }

    /**
     * sets the ar value and its components (id, p1, p2, e_left, e_right,
     * midSideNode)
     * 
     * @param sim
     */
    private static void setAr(Simulation2D sim) {
        List arList = sim.getAr();
        Simulation2DTypeImpl.ArTypeImpl arType = new Simulation2DTypeImpl.ArTypeImpl();
        for (int i = 0; i < arList.size(); ++i) {
            arType = (Simulation2DTypeImpl.ArTypeImpl) arList.get(i);
            p1[i] = arType.getP1().intValue();
            p2[i] = arType.getP2().intValue();
            e_left[i] = arType.getELeft().intValue();
            e_right[i] = arType.getERight().intValue();
            if (arType.getMidSideNode() != null) {
                e_midSideNode[i] = arType.getMidSideNode().intValue();
                ar[i][5] = "" + e_midSideNode[i];
            } else
                ar[i][5] = "" + 0;

            ar[i][1] = "" + p1[i];
            ar[i][2] = "" + p2[i];
            ar[i][3] = "" + e_left[i];
            ar[i][4] = "" + e_right[i];
        }

    }

    /**
     * sets the parameter of fe (id, roughnessOld, roughnessInTimeWarp,
     * orderOfProcessing,defaultMass)
     * 
     * @param sim
     */
    private static void setFE(Simulation2D sim) {
        List feList = sim.getFe();
        Simulation2DTypeImpl.FeTypeImpl feType = new Simulation2DTypeImpl.FeTypeImpl();
        sizeElements = feList.size();
        for (int i = 0; i < feList.size(); ++i) {
            feType = (Simulation2DTypeImpl.FeTypeImpl) feList.get(i);
            if (feType.getId() != null)
                feId[i] = feType.getId().intValue();

            if (feType.getRoughnessOld() != null)
                ro[i] = feType.getRoughnessOld().intValue();

            if (feType.getRoughnessInTimeWarp() != null)
                rtw[i] = feType.getRoughnessInTimeWarp().intValue();

            if (feType.getOrderOfProcessing() != null)
                oop[i] = feType.getOrderOfProcessing().intValue();

            dm[i] = feType.getDefaultMass();
        }
    }

    /**
     * sets the rk parameters (id, roughness, color, param1, param2, param3,
     * param4)
     * 
     * @param sim
     */
    private static void setRK(Simulation2D sim) {
        List list = sim.getRk();
        Simulation2DTypeImpl.RkTypeImpl type = new Simulation2DTypeImpl.RkTypeImpl();
        for (int i = 0; i < list.size(); i++) {
            type = (Simulation2DTypeImpl.RkTypeImpl) list.get(i);
            int j = i + 1;
            roughness[j] = type.getRoughness();
            color[j] = type.getColor();
            if (type.getParam1() != null)
                param1[j] = type.getParam1().intValue();
            if (type.getParam2() != null)
                param2[j] = type.getParam2().intValue();
            if (type.getParam3() != null)
                param3[j] = type.getParam3().intValue();
            if (type.getParam4() != null)
                param4[j] = type.getParam4().intValue();
        }
    }

    /**
     * sets the fp parameters (fp -> id, geometry(x-,y-,z-coordinate) )
     * 
     * @param type
     * @param i
     */
    private static void setFP(FpParams.FpType type, int i) {
        coordX[i] = m_xoffset + type.getGeometry().getX();
        coordY[i] = m_yoffset + type.getGeometry().getY();
        coordZ[i] = type.getGeometry().getZ();
    }

    /**
     * sets the parameters of va (xVelocity, yVelocity, riverDepth, waterLevel )
     */
    private static void setVA(FpParams.VaType vaType, int i) {
        xVelocity[i] = vaType.getXVelocity();
        yVelocity[i] = vaType.getYVelocity();
        riverDepth[i] = vaType.getRiverDepth();
        waterLevel[i] = vaType.getWaterLevel();
    }

    /**
     * sets the ga parameters (timeGradient1, timeGradient2, timeGradient3 )
     */
    private static void setGA(FpParams.GaType gaType, int i) {
        timeGradient1[i] = gaType.getTimeGradient1();
        timeGradient2[i] = gaType.getTimeGradient2();
        timeGradient3[i] = gaType.getTimeGradient3();
    }

    /**
     * sets the parameters of vo (degreeOfFreedomOfOldTimeGradient1,
     * degreeOfFreedomOfOldTimeGradient2, degreeOfFreedomOfOldTimeGradient3)
     *  
     */
    private static void setVO(FpParams.VoType voType, int i) {
        degreeOfFreedomOfOldTimeGradient1[i] = voType
                .getDegreeOfFreedomOfOldTimeGradient1();
        degreeOfFreedomOfOldTimeGradient2[i] = voType
                .getDegreeOfFreedomOfOldTimeGradient2();
        degreeOfFreedomOfOldTimeGradient3[i] = voType
                .getDegreeOfFreedomOfOldTimeGradient3();
    }

    /**
     * sets the parameters of go (TimeGradientOfFormerTimeWarps1,
     * TimeGradientOfFormerTimeWarps2, TimeGradientOfFormerTimeWarps3)
     */
    private static void setGO(FpParams.GoType goType, int i) {
        timeGradientOfFormerTimeWarps1[i] = goType
                .getTimeGradientOfFormerTimeWarps1();
        timeGradientOfFormerTimeWarps2[i] = goType
                .getTimeGradientOfFormerTimeWarps2();
        timeGradientOfFormerTimeWarps3[i] = goType
                .getTimeGradientOfFormerTimeWarps3();
    }

    /**
     * sets the parameters of zu (info1, info2, info3, info4, info5,info6)
     */
    private static void setZU(FpParams.ZuType zuType, int i) {
        info1[i] = zuType.getInfo1();
        info2[i] = zuType.getInfo2();
        info3[i] = zuType.getInfo3();
        info4[i] = zuType.getInfo4();
        info5[i] = zuType.getInfo5();
        info6[i] = zuType.getInfo6();
    }

    /**
     * sets the fpInfo params or rather calls the nodes to be set containing the
     * paramertes of the subnodes (va, ga, vo, go and zu)
     * 
     * @param sim
     */
    private static void setFPInfo(Simulation2D sim, boolean exists) {
        List list = sim.getFpInfo();
        sizeFP = list.size();

        for (int i = 0; i < list.size(); ++i) {
            FpParamsImpl fpinfo = (FpParamsImpl) list.get(i);
            FpParams.FpType typeFp = fpinfo.getFp();

            try {
                setFP(typeFp, i);
            } catch (Exception ex) {
                System.out.println("could not create node");
            }
            if (exists == true) {
                FpParams.GaType typeGa = fpinfo.getGa();
                FpParams.GoType typeGo = fpinfo.getGo();
                FpParams.VaType typeVa = fpinfo.getVa();
                FpParams.VoType typeVo = fpinfo.getVo();
                FpParams.ZuType typeZu = fpinfo.getZu();
                setGA(typeGa, i);
                setVA(typeVa, i);
                setGO(typeGo, i);
                setVO(typeVo, i);
                setZU(typeZu, i);
            }
        }
    }

    /**
     * creates the node of feature points
     * 
     * @param doc
     * @param e
     * @param i
     */
    private static void createFPParamsNode(Document doc, Element e, int i,
            boolean exists) {

        try {
            Element geom = doc.createElement("geometry");
            e.appendChild(geom);

            Element point = doc.createElement("gml:Point");
            point.setAttribute("srsName",
                    "http://www.opengis.net/gml/srs/epsg.xml#31467");
            geom.appendChild(point);
            Element coord = doc.createElement("gml:coord");
            point.appendChild(coord);

            Element x = doc.createElement("gml:X");
            x.appendChild(XMLHelper.createTextNode(doc, "" + coordX[i]));
            Element y = doc.createElement("gml:Y");
            y.appendChild(XMLHelper.createTextNode(doc, "" + coordY[i]));
            Element z = doc.createElement("gml:Z");
            z.appendChild(XMLHelper.createTextNode(doc, "" + coordZ[i]));

            coord.appendChild(x);
            coord.appendChild(y);
            coord.appendChild(z);
            e.appendChild(geom);
        } catch (Exception ex) {
            System.out
                    .println("Error occurred in generating geometry element by converting to gml file");
        }

        if (exists == true) {
            Element xVel = doc.createElement("xVelocity");
            xVel.appendChild(XMLHelper.createTextNode(doc, "" + xVelocity[i]));
            e.appendChild(xVel);

            Element yVel = doc.createElement("yVelocity");
            yVel.appendChild(XMLHelper.createTextNode(doc, "" + yVelocity[i]));
            e.appendChild(yVel);

            Element rd = doc.createElement("riverDepth");
            rd.appendChild(XMLHelper.createTextNode(doc, "" + riverDepth[i]));
            e.appendChild(rd);

            Element wl = doc.createElement("waterLevel");
            wl.appendChild(XMLHelper.createTextNode(doc, "" + waterLevel[i]));
            e.appendChild(wl);

            Element tg1 = doc.createElement("timeGradient1");
            tg1.appendChild(XMLHelper
                    .createTextNode(doc, "" + timeGradient1[i]));
            e.appendChild(tg1);

            Element tg2 = doc.createElement("timeGradient2");
            tg2.appendChild(XMLHelper
                    .createTextNode(doc, "" + timeGradient2[i]));
            e.appendChild(tg2);

            Element tg3 = doc.createElement("timeGradient3");
            tg3.appendChild(XMLHelper
                    .createTextNode(doc, "" + timeGradient3[i]));
            e.appendChild(tg3);

            Element dfoog1 = doc
                    .createElement("degreeOfFreedomOfOldTimeGradient1");
            dfoog1.appendChild(XMLHelper.createTextNode(doc, ""
                    + degreeOfFreedomOfOldTimeGradient1[i]));
            e.appendChild(dfoog1);

            Element dfoog2 = doc
                    .createElement("degreeOfFreedomOfOldTimeGradient2");
            dfoog2.appendChild(XMLHelper.createTextNode(doc, ""
                    + degreeOfFreedomOfOldTimeGradient2[i]));
            e.appendChild(dfoog2);

            Element dfoog3 = doc
                    .createElement("degreeOfFreedomOfOldTimeGradient3");
            dfoog3.appendChild(XMLHelper.createTextNode(doc, ""
                    + degreeOfFreedomOfOldTimeGradient3[i]));
            e.appendChild(dfoog3);

            Element tgtw1 = doc.createElement("TimeGradientOfFormerTimeWarps1");
            tgtw1.appendChild(XMLHelper.createTextNode(doc, ""
                    + timeGradientOfFormerTimeWarps1[i]));
            e.appendChild(tgtw1);

            Element tgtw2 = doc.createElement("TimeGradientOfFormerTimeWarps2");
            tgtw2.appendChild(XMLHelper.createTextNode(doc, ""
                    + timeGradientOfFormerTimeWarps2[i]));
            e.appendChild(tgtw2);

            Element tgtw3 = doc.createElement("TimeGradientOfFormerTimeWarps3");
            tgtw3.appendChild(XMLHelper.createTextNode(doc, ""
                    + timeGradientOfFormerTimeWarps3[i]));
            e.appendChild(tgtw3);
        }
    }

    /**
     * gets the x,y and z coordinate for the featurePoint with the given id
     * 
     * @param id
     * @return <double>
     */
    public static double[] getCoordinates(int id) {
        id = id - 1;
        if (id >= 0) {
            double coordx = coordX[id];
            double coordy = coordY[id];
            double coordz = coordZ[id];

            double[] coords = new double[] { coordx, coordy, coordz };

            return coords;
        } else
            return null;
    }

    /**
     * sets the polygons of each element at i
     * 
     * @param sim
     */
    private static void setPolygonOfElements(Simulation2D sim) {
        List arList = sim.getAr();
        for (int i = 0; i < arList.size(); i++) {

            int meshPolygonIdLeft = (int) Math.round(Double
                    .parseDouble(ar[i][3]));
            int meshPolygonIdRight = (int) Math.round(Double
                    .parseDouble(ar[i][4]));

            MeshPolygon myMeshPolygon = null;

            if (outPolygons.size() == 0) {
                myMeshPolygon = new MeshPolygon();
                myMeshPolygon.setPoint((int) Math.round(Double
                        .parseDouble(ar[i][1])));
                myMeshPolygon.setPoint((int) Math.round(Double
                        .parseDouble(ar[i][2])));

                if (outPolygons.size() <= meshPolygonIdLeft)
                    outPolygons.setSize(meshPolygonIdLeft + 1);
                outPolygons.setElementAt(myMeshPolygon, meshPolygonIdLeft);
            }
            if (outPolygons.size() > 0 && meshPolygonIdLeft > 0) {
                if ((outPolygons.size() - 1) < meshPolygonIdLeft)
                    outPolygons.setSize(meshPolygonIdLeft + 1);
                if (outPolygons.elementAt(meshPolygonIdLeft) != null) {
                    myMeshPolygon = (MeshPolygon) outPolygons
                            .elementAt(meshPolygonIdLeft);
                    myMeshPolygon.setPoint((int) Math.round(Double
                            .parseDouble(ar[i][1])));
                    myMeshPolygon.setPoint((int) Math.round(Double
                            .parseDouble(ar[i][2])));

                    myMeshPolygon.setAR((int) Math.round(Double
                            .parseDouble(ar[i][3])), (int) Math.round(Double
                            .parseDouble(ar[i][1])), (int) Math.round(Double
                            .parseDouble(ar[i][2])));
                } else {
                    myMeshPolygon = new MeshPolygon();
                    myMeshPolygon.setPoint((int) Math.round(Double
                            .parseDouble(ar[i][1])));
                    myMeshPolygon.setPoint((int) Math.round(Double
                            .parseDouble(ar[i][2])));
                    outPolygons.setElementAt(myMeshPolygon, meshPolygonIdLeft);

                    myMeshPolygon.setAR((int) Math.round(Double
                            .parseDouble(ar[i][3])), (int) Math.round(Double
                            .parseDouble(ar[i][1])), (int) Math.round(Double
                            .parseDouble(ar[i][2])));
                }
            }

            if (outPolygons.size() > 0 && meshPolygonIdRight > 0) {
                if (meshPolygonIdRight > (outPolygons.size() - 1)) {
                    outPolygons.setSize(meshPolygonIdRight + 1);
                }
                if (outPolygons.elementAt(meshPolygonIdRight) != null) {
                    myMeshPolygon = (MeshPolygon) outPolygons
                            .elementAt(meshPolygonIdRight);
                    myMeshPolygon.setPoint((int) Math.round(Double
                            .parseDouble(ar[i][1])));
                    myMeshPolygon.setPoint((int) Math.round(Double
                            .parseDouble(ar[i][2])));

                    myMeshPolygon.setAR((int) Math.round(Double
                            .parseDouble(ar[i][4])), (int) Math.round(Double
                            .parseDouble(ar[i][1])), (int) Math.round(Double
                            .parseDouble(ar[i][2])));
                } else {
                    myMeshPolygon = new MeshPolygon();
                    myMeshPolygon.setPoint((int) Math.round(Double
                            .parseDouble(ar[i][1])));
                    myMeshPolygon.setPoint((int) Math.round(Double
                            .parseDouble(ar[i][2])));
                    outPolygons.setElementAt(myMeshPolygon, meshPolygonIdRight);

                    myMeshPolygon.setAR((int) Math.round(Double
                            .parseDouble(ar[i][4])), (int) Math.round(Double
                            .parseDouble(ar[i][1])), (int) Math.round(Double
                            .parseDouble(ar[i][2])));
                }
            }
        }
    }

    /**
     * sets the polygon of the elements (FE)
     * 
     * @param doc
     * @param e
     * @param i
     */
    private static void createPolygonNode(Document doc, Element e, int i) {
        Element polygonPropElement = doc.createElement("gml:polygonProperty");
        e.appendChild(polygonPropElement);

        Element polygonElement = doc.createElement("gml:Polygon");
        //TODO dynamic srs
        polygonElement.setAttribute("srsName",
                "http://www.opengis.net/gml/srs/epsg.xml#31467");
        polygonPropElement.appendChild(polygonElement);

        Element outerB = doc.createElement("gml:outerBoundaryIs");
        polygonElement.appendChild(outerB);
        Element linearR = doc.createElement("gml:LinearRing");
        outerB.appendChild(linearR);

        String coordinates = "";
        double[] coordsPoint1 = new double[3];
        double[] coordsPoint2 = new double[3];
        double[] coordsPoint3 = new double[3];
        double[] coordsPoint4 = new double[3];

        try {
            MeshPolygon mp = (MeshPolygon) outPolygons.elementAt(i);
            String coordinatesP1 = "";
            String coordinatesP2 = "";
            String coordinatesP3 = "";
            if (mp != null) {
                int[] idMP = mp.getPointIDs();

                if (coordsPoint1 != null && coordsPoint2 != null
                        && coordsPoint3 != null && idMP[2] != 0.0) {
                    coordsPoint1 = getCoordinates(idMP[0]);
                    coordsPoint2 = getCoordinates(idMP[1]);
                    coordsPoint3 = getCoordinates(idMP[2]);

                    coordinatesP1 = coordsPoint1[0] + "," + coordsPoint1[1]
                            + "," + coordsPoint1[2];
                    coordinatesP2 = coordsPoint2[0] + "," + coordsPoint2[1]
                            + "," + coordsPoint2[2];
                    coordinatesP3 = coordsPoint3[0] + "," + coordsPoint3[1]
                            + "," + coordsPoint3[2];

                    coordinates = coordinatesP1 + " " + coordinatesP2 + " "
                            + coordinatesP3 + " " + coordinatesP1;
                }

                if (mp.getVector().size() == 4) {
                    String[] orderOfEdges = mp.getOrderOfEdges();
                    int p1 = (int) Double.parseDouble(orderOfEdges[0]);
                    int p2 = (int) Double.parseDouble(orderOfEdges[1]);
                    int p3 = (int) Double.parseDouble(orderOfEdges[2]);
                    int p4 = (int) Double.parseDouble(orderOfEdges[3]);

                    coordsPoint1 = getCoordinates(p1);
                    coordsPoint2 = getCoordinates(p2);
                    coordsPoint3 = getCoordinates(p3);
                    coordsPoint4 = getCoordinates(p4);

                    coordinatesP1 = coordsPoint1[0] + "," + coordsPoint1[1]
                            + "," + coordsPoint1[2];
                    coordinatesP2 = coordsPoint2[0] + "," + coordsPoint2[1]
                            + "," + coordsPoint2[2];
                    coordinatesP3 = coordsPoint3[0] + "," + coordsPoint3[1]
                            + "," + coordsPoint3[2];

                    String coordinatesP4 = "";
                    coordinatesP4 = coordsPoint4[0] + "," + coordsPoint4[1]
                            + "," + coordsPoint4[2];

                    coordinates = coordinatesP1 + " " + coordinatesP4 + " "
                            + coordinatesP3 + " " + coordinatesP2 + " "
                            + coordinatesP1;
                }
            }
            if (coordinates.equalsIgnoreCase("")) {
                System.out
                        .println("m"
                                + i
                                + " has no edges: error in ascii file defining this feature");
                removeElem = i;
            } 

        } catch (Exception ex) {
            System.out.println("Error occurred in createPolygonNode ");
            ex.printStackTrace();
        }

        Element coord = doc.createElement("gml:coordinates");
        coord.appendChild(XMLHelper.createTextNode(doc, coordinates));
        linearR.appendChild(coord);

        e.appendChild(polygonPropElement);
    }

    /**
     * sets the nodes for the elements (FE)
     * 
     * @param doc
     * @param e
     * @param i
     */
    private static void createElementsNode(Document doc, Element e, int i) {

        try {
            Element idElem = doc.createElement("feId");
            idElem.appendChild(XMLHelper.createTextNode(doc, "" + feId[i]));
            e.appendChild(idElem);

            createPolygonNode(doc, e, feId[i]);

            Element r = doc.createElement("roughness");
            r.appendChild(XMLHelper.createTextNode(doc, roughness[ro[i]]));
            e.appendChild(r);

            Element c = doc.createElement("color");
            c.appendChild(XMLHelper.createTextNode(doc, color[ro[i]]));
            e.appendChild(c);

            Element roElem = doc.createElement("roughnessOld");
            roElem.appendChild(XMLHelper.createTextNode(doc, ""
                    + roughness[ro[i]]));
            e.appendChild(roElem);

            Element rtwElem = doc.createElement("roughnessInTimeWarp");
            rtwElem.appendChild(XMLHelper.createTextNode(doc, ""
                    + roughness[rtw[i]]));
            e.appendChild(rtwElem);

            Element oopElem = doc.createElement("orderOfProcessing");
            oopElem.appendChild(XMLHelper.createTextNode(doc, "" + oop[i]));
            e.appendChild(oopElem);

            Element dmElem = doc.createElement("defaultMass");
            dmElem.appendChild(XMLHelper.createTextNode(doc, "" + dm[i]));
            e.appendChild(dmElem);
        } catch (Exception ex) {
            System.out
                    .println("error in roughness parameter: negativ values in ascii file");
        }
    }

    /**
     * creates the gml document
     * 
     * @param outFile
     */
    private static void createGML(String outFile, boolean exists) {
        Document doc = XMLHelper.createDocument();
        try {
            doc.createTextNode("<?xml version=\"1.0\" encoding=\"UTF-8\"?>");
            Element e = doc.createElement("simulation2d");
            e.setAttributeNode(XMLHelper.createAttribute(doc, "xmlns", NS));
            e.setAttributeNode(XMLHelper.createAttribute(doc, "xmlns:gml",
                    GML_NS));
            e.setAttributeNode(XMLHelper.createAttribute(doc, "xmlns:xlink",
                    XLINK_NS));
            e.setAttributeNode(XMLHelper.createAttribute(doc, "xmlns:xsi",
                    XSI_NS));
            e.setAttributeNode(XMLHelper.createAttribute(doc,
                    "xsi:schemaLocation", XSI_schemaLocation_NS));
            doc.appendChild(e);

            /*
             * feature point
             */
            Element fpCollMember = doc
                    .createElement("featurePointCollectionMember");
            e.appendChild(fpCollMember);

            Element pointsColl = doc.createElement("pointsCollection");
            fpCollMember.appendChild(pointsColl);
            System.out.println("KNOTEN " + sizeFP);
            for (int i = 0; i < sizeFP; i++) {
                Element fpMember = doc.createElement("featurePointMember");
                pointsColl.appendChild(fpMember);

                Element fpElem = doc.createElement("featurePoint");
                fpElem.setAttribute("fid", "p" + i);
                fpMember.appendChild(fpElem);
                createFPParamsNode(doc, fpElem, i, exists);
            }

            /*
             * fem
             */
            Element femCollMember = doc.createElement("femCollectionMember");
            e.appendChild(femCollMember);

            Element meshColl = doc.createElement("meshCollection");
            femCollMember.appendChild(meshColl);

            System.out.println("MESH " + sizeElements);
            for (int j = 0; j < sizeElements; j++) {

                Element meshMember = doc.createElement("meshMember");
                meshColl.appendChild(meshMember);

                Element meshElement = doc.createElement("femMesh");
                //	            meshElement.setAttribute("fid", "m"+j);
                meshMember.appendChild(meshElement);
                createElementsNode(doc, meshElement, j);

                if ((j + 1) == removeElem) {
                    meshColl.removeChild(meshMember);
                }
            }

            OutputStreamWriter writer = new OutputStreamWriter(
                    new FileOutputStream(outFile), "UTF-8");

            final Transformer t = TransformerFactory.newInstance()
                    .newTransformer();

            t.setOutputProperty("{http://xml.apache.org/xslt}indent-amount",
                    "2");
            t.setOutputProperty(OutputKeys.INDENT, "yes");

            t.transform(new DOMSource(e), new StreamResult(writer));
            writer.close();

        } catch (Exception ex) {
            ex.printStackTrace();
        }

    }

    /**
     * starts transformation from xml to gml
     * 
     * @param inFile
     * @param outFile
     * @param existsDetailedFP
     */
    public void startXML2GML(String inFile, String outFile,
            boolean existsDetailedFP) {
        try {
            ObjectFactory fac = new ObjectFactory();
            Unmarshaller unmarshaller = fac.createUnmarshaller();

            Simulation2D sim = (Simulation2D) unmarshaller.unmarshal(new File(
                    inFile));
            setFPInfo(sim, existsDetailedFP);
            setAr(sim);
            setFE(sim);
            setRK(sim);
            setPolygonOfElements(sim);
            createGML(outFile, existsDetailedFP);
        } catch (Exception e) {
            System.out
                    .println("Error in converting xml file to gml file -> JAXB Exception: ");
            e.printStackTrace();
        }
    }

}