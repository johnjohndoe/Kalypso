package de.tuhh.wb.javagis.view.singleview;


import java.awt.event.ActionListener;

import javax.swing.JButton;

import de.tuhh.wb.javagis.data.GisElement;


public class GisSingleObjectLink extends JButton implements ActionListener
{
    private GisElement myGisElement;
    private GisSingleObjectView myView;
    public GisSingleObjectLink(GisSingleObjectView view,String label,GisElement gisElement)
    {
	super(label);
	this.myGisElement=gisElement;
	this.myView=view;
	addActionListener(this);
    }
    
    public void actionPerformed(java.awt.event.ActionEvent e)
    {
	String frameName="SingeleObjectView";
	GisSingleObjectView.load("Kalypso",myGisElement);
    }
}

