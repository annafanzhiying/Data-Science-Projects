% Purpose: 
% QFE term paper 
% Author: 
% Zhiying Fan, 04/30/2019
% Note:
% This m-file is dependent upon 'SP400_components.xlsx','SP400_GIC.xlsx',
% 'SP500_raw.xlsx','SP500_MV.xlsx','QFE_ticker.xlsx','stockshare.xlsx'
% 'SP400_MV.xlsx','SP500_components.xlsx','SP500_GIC.xlsx','SP500_pMV.xlsx'files
% UNC Honor Pledge: I certify that no unauthorized assistance has been received or given in the completion of this work.
%% Section 1: Get SP400 peers
%Housekeeping
    clear all; close all; clc
% Import SP400 selected tickers, data extracted from WRDS
% This file contains historical continuents of SP400 2006/01 - 2019/03 
% and its corresponding effective dates
    SP400_components = readtable('SP400_components.xlsx');
    % Some EffectiveThruDate contains NaN meaning that those stocks have
    % never been deleted; set those NaN to 0 for convenience
    SP400_components.EffectiveThruDate(isnan(SP400_components.EffectiveThruDate)) = 0;
% This is SP400 ccontinuents with available GIC sub-industry code, unavaiable
% GIC data should imply events such as M&A or bankruptcy of the firms
    SP400_GIC = readtable('SP400_GIC.xlsx'); 
% Delete the last 4 digits of GIC sub-industry to get GIC industry group 
    SP400_GIC.GIC = floor(SP400_GIC.GIC/10000);
% Match stock GIC codes with effective date data
% Those without GIC codes will be first excluded from study 
    % Set cols with zeros to store data 
    SP400_GIC.EffectiveFrom = zeros(1154,1);
    SP400_GIC.EffectiveThru = zeros(1154,1);
    % loop
    for i = 1:1154 
        % locate ticker from SP400_GIC in SP400_components
        idx = find(strcmp(SP400_components.CompanyTicker,SP400_GIC.Ticker(i)));
        % transfer thier foundamental data to SP400_GIC
        SP400_GIC.EffectiveFrom(i) = SP400_components.EffectiveFromDate(idx);
        SP400_GIC.EffectiveThru(i) = SP400_components.EffectiveThruDate(idx);
    end 
    clear i; clear idx
%% Process SP500 data
% Import SP500 deleted stocks with GIC codes
% List extracted from WRDS and edited in excel to exclude those with
% unavailable foundamental information (due to events such as M&A)
    SP500raw = readtable('SP500_raw.xlsx');
    % Delete the last 2 digits of GIC to get GIC industry group 
    SP500raw.GIC = floor(SP500raw.GIC/100);
    % get the list of GIC of SP500 deletions with no repetitions
    GIC = unique(SP500raw.GIC);
    % find the SP400 continuents with first GIC code in the GIClist
    idx = find(GIC(1) == SP400_GIC.GIC);
    % I don't want SP400 peers that are already deleted before their SP500
    % peers. As the first step of peer screening, I only want to keep those 
    % SP400 with effectivethru dates at least 1 year later than the 
    % minimum effectivethru date of every GIC group on SP500. 
    SP400 = SP400_GIC(idx,:);
    mindate = min(SP500raw.AnnouncementDate(SP500raw.GIC == (GIC(1)))) + 10000;
    idx1 = find(SP400.EffectiveThru > mindate | SP400.EffectiveThru == 0);
    SP400 = SP400(idx1,:);
    % loop over the remaining GIC codes with same steps as above
    for i = 2:length(GIC)
        idx = find(GIC(i) == SP400_GIC.GIC);
        SP400temp = SP400_GIC(idx,:);
        mindate = min(SP500raw.AnnouncementDate(SP500raw.GIC == (GIC(i)))) + 10000;
        idx1 = find(SP400temp.EffectiveThru > mindate | SP400temp.EffectiveThru == 0);
        SP400temp = SP400temp(idx1,:);
        SP400 = [SP400; SP400temp];
    end 
    clear i; clear idx; clear idx1; clear SP400temp;

% Import market capitals (from WRDS) of stocks in the SP500 list
% Note the market capital data is denoted as market value in the file,
% while WRDS describes 'market value' as shares outstanding*price in millions
    SP500raw_mv = readtable('SP500_MV.xlsx');
% Translate annoucement dates in SP500raw to year/quarter for comaprability
% Ad_str = num2str(SP500raw.AnnouncementDate);
    SP500raw.date = SP500raw.AnnouncementDate - floor(SP500raw.AnnouncementDate/10000)*10000;
    % Assign quarter indicator to first date value 
    a = 'Q2';
    for i = 2:height(SP500raw)
        if SP500raw.date(i) <= 331 %first quarter cutoff
            q = 'Q1';
        else if SP500raw.date(i) <= 630 %second quarter cutoff
                q = 'Q2';
            else if SP500raw.date(i) <= 930 %third quarter cutoff
                    q = 'Q3';
                else q = 'Q4'; % all other belong to forth quarter 
                end 
            end
        end 
        a = [a; q];
    end 
% Concatenate annoucement year with quarter indicator 
    % convert Announcement year to string 
    ADtemp = num2str(floor(SP500raw.AnnouncementDate/10000));
    ADqtemp = strcat(ADtemp,a);
    SP500raw.ADQ = cellstr(ADqtemp); % save quarter to SP500raw table
    SP500raw.date = []; % delete the date date
% Find market cap of SP500raw tickers in the quarter when we get deleted 
    SP500raw.MarketCap = zeros(height(SP500raw),1);
    for i = 1:height(SP500raw)
        idx = find(strcmp(SP500raw_mv.TickerSymbol,SP500raw.Ticker(i)));
        temp = SP500raw_mv(idx,:);
        idx1 = strcmp(temp.CalendarDataYearAndQuarter,SP500raw.ADQ(i));
        if ~isempty(temp.MarketValue_Total(idx1))
            SP500raw.MarketCap(i) = temp.MarketValue_Total(idx1);
        end 
    end 
    clear temp; clear idx; clear idx1;
%% Sort peers from SP400
    % export SP400 list to txt to extract market cap data in WRDS
    writetable(SP400(:,1),'SP400list');
    SP400mv = readtable('SP400_MV.xlsx');
    SP500raw.peer4 = SP500raw.Ticker;
    SP500raw.peer42 = SP500raw.Ticker;
    SP500raw.peer43 = SP500raw.Ticker;
    SP400mv(isnan(SP400mv.MarketValue_Total),:) = [];
    % match SP500 for the first SP400
    for i = 1: height(SP500raw)
        temp = SP400(SP400.GIC == SP500raw.GIC(i),:);
        temp = temp(temp.EffectiveFrom < SP500raw.AnnouncementDate(i) & (temp.EffectiveThru > SP500raw.AnnouncementDate(i) | temp.EffectiveThru == 0),:);
        temp.mv = zeros(height(temp),1);
            for j = 1:height(temp)
                l = strcmp(SP400mv.TickerSymbol,temp.Ticker(j)) & strcmp(SP400mv.CalendarDataYearAndQuarter,SP500raw.ADQ(i));
                a = SP400mv.MarketValue_Total(l);
                if isempty(a) 
                    a = 0;
                end 
                temp.mv(j) = a;
            end 
            temp(temp.mv == 0,:) =[];
            diffmv = (temp.mv - SP500raw.MarketCap(i)).^2;
            if length(diffmv) > 1
                diffmvs = sort(diffmv);
                peer2 = temp.Ticker(find(diffmv == diffmvs(2)));
                SP500raw.peer42(i) = peer2;
                if length(diffmv) > 2
                    peer3 = temp.Ticker(find(diffmv == diffmvs(3)));
                    SP500raw.peer43(i) = peer3;
                end 
            end 
            peer = temp.Ticker(find(diffmv == min(diffmv)));
            SP500raw.peer4(i) = peer;  
    end 
    % checked that there are dups in first choice peer based on mkt cap
    uniquepeer = unique(SP500raw.peer4);
    SP500raw.peer4final = SP500raw.peer4;
    % loop to assign second choice peers to those with dups
    for i = 1:length(uniquepeer)
        l = strcmp(SP500raw.peer4,uniquepeer(i));
        if sum(l) > 1
            for j = 2:sum(l)
                [row,col] = find(l);
                SP500raw.peer4final(row(j)) = SP500raw.peer42(row(j));
            end 
        end 
    end 
    % Manually adjust four other dups with third choice peer
    SP500raw = sortrows(SP500raw,'peer4final','ascend');
    SP500raw.peer4final(16) = SP500raw.peer43(16);
    SP500raw.peer4final(48) = SP500raw.peer43(48);
    SP500raw.peer4final(61) = SP500raw.peer43(61);
    SP500raw.peer4final(51) = SP500raw.peer43(51); 
%% Section 2: Sort SP500 peers
% Import SP400 selected tickers, data extracted from WRDS
% This file contains historical continuents of SP500 2006/01 - 2019/03 
% and its corresponding effective dates
    SP500_components = readtable('SP500_components.xlsx');
    % Some EffectiveThruDate contains NaN meaning that those stocks have
    % never been deleted; set those NaN to 0 for convenience
    SP500_components.EffectiveThruDate(isnan(SP500_components.EffectiveThruDate)) = 0;
    writetable(SP500_components(:,6),'SP500list'); % for wrds
% This is SP500 ccontinuents with available GIC sub-industry code, unavaiable
% GIC data should imply events such as M&A or bankruptcy of the firms
    SP500_GIC = readtable('SP500_GIC.xlsx'); 
% Match stock GIC codes with effective date data
% Those without GIC codes will be first excluded from study 
    % Set cols with zeros to store data 
    SP500_GIC.EffectiveFrom = zeros(height(SP500_GIC),1);
    SP500_GIC.EffectiveThru = zeros(height(SP500_GIC),1);
    % loop
    for i = 1: height(SP500_GIC)
        % locate ticker from SP500_GIC in SP500_components
        idx = find(strcmp(SP500_components.CompanyTicker,SP500_GIC.Ticker(i)));
        % transfer thier foundamental data to SP400_GIC
        SP500_GIC.EffectiveFrom(i) = SP500_components.EffectiveFromDate(idx);
        SP500_GIC.EffectiveThru(i) = SP500_components.EffectiveThruDate(idx);
    end 
    clear i; clear idx
    % Delete SP500 peer list that duplicate with treatment group
    for i = 1:height(SP500raw)
        idx = find(strcmp(SP500_GIC.Ticker,SP500raw.Ticker(i)));
        SP500_GIC(idx,:) = [];
    end 
    clear i; clear idx
    % get only SP500 peers within industry screening range 
    idx = find(GIC(1) == SP500_GIC.GIC);
    SP500p = SP500_GIC(idx,:);
    for i = 2:length(GIC)
        idx = find(GIC(i) == SP500_GIC.GIC);
        SP500temp = SP500_GIC(idx,:);
        SP500p = [SP500p; SP500temp];
    end 
    clear i; clear idx; clear idx1; 
% export SP500 list to txt to extract market cap data in WRDS
    writetable(SP500p(:,1),'SP500list2');
%% Import WRDS mkt cap
    SP500pmv = readtable('SP500_pMV.xlsx');
    % delete inactive stocks
    SP500pmv = SP500pmv(strcmp(SP500pmv.Active_InactiveStatusMarker,'A'),:);
    SP500pmv(isnan(SP500pmv.MarketValue_Total),:) = [];
    % match SP500 for the first SP500 peer
    for i = 1: height(SP500raw)
        temp = SP500p(SP500p.GIC == SP500raw.GIC(i),:);
        temp = temp(temp.EffectiveFrom < SP500raw.AnnouncementDate(i) - 20000 & (temp.EffectiveThru > SP500raw.AnnouncementDate(i) + 20000 | temp.EffectiveThru == 0),:);
        temp.mv = zeros(height(temp),1);
            for j = 1:height(temp)
                l = strcmp(SP500pmv.TickerSymbol,temp.Ticker(j)) & strcmp(SP500pmv.CalendarDataYearAndQuarter,SP500raw.ADQ(i));
                a = SP500pmv.MarketValue_Total(l);
                if isempty(a) 
                    a = 0;
                end 
                temp.mv(j) = a;
            end 
            temp(temp.mv == 0,:) =[];
            diffmv = (temp.mv - SP500raw.MarketCap(i)).^2; %find least squared difference in market value 
            diffmvs = sort(diffmv);
            peer = temp.Ticker(find(diffmv == min(diffmv)));
            SP500raw.peer5(i) = peer; 
            peer2 = temp.Ticker(find(diffmv == diffmvs(2))); % second peer group
            SP500raw.peer52(i) = peer2;
            peer3 = temp.Ticker(find(diffmv == diffmvs(3)));
            SP500raw.peer53(i) = peer3; % third peer group
            if length(diffmvs) > 3
            peer4 = temp.Ticker(find(diffmv == diffmvs(4)));
            SP500raw.peer54(i) = peer4;% forth peer group
            end 
            if length(diffmvs) > 4
                peer5 = temp.Ticker(find(diffmv == diffmvs(5)));
                SP500raw.peer55(i) = peer5; % fifth peer group
            end 
            if length(diffmvs) > 5
                peer6 = temp.Ticker(find(diffmv == diffmvs(6)));
                SP500raw.peer56(i) = peer6; % sixth peer group
            end  
            if length(diffmvs) > 6
                peer7 = temp.Ticker(find(diffmv == diffmvs(7)));
                SP500raw.peer57(i) = peer7; % sixth peer group
            end  
            if length(diffmvs) > 7
                peer8 = temp.Ticker(find(diffmv == diffmvs(8)));
                SP500raw.peer58(i) = peer8; % sixth peer group
            end  
    end 
    SP500raw = sortrows(SP500raw,'peer5','ascend');
    % checked that there are dups in first choice peer based on mkt cap
    unique5 = unique(SP500raw.peer5);
    SP500raw.peer5final = SP500raw.peer5;
    % loop to assign second choice peers to those with dups
    % if dup --> use the peer from the next peer group 
    for i = 1:length(unique5)
        l = strcmp(SP500raw.peer5,unique5(i));
        if sum(l) > 1
            for j = 2:sum(l)
                [row,col] = find(l);
                SP500raw.peer5final(row(j)) = SP500raw.peer52(row(j));
            end 
        end 
    end 
    unique55 = unique(SP500raw.peer5final);
    for i = 1:length(unique55)
        l = strcmp(SP500raw.peer5final,unique55(i));
        if sum(l) > 1
            for j = 2:sum(l)
                [row,col] = find(l);
                SP500raw.peer5final(row(j)) = SP500raw.peer53(row(j));
            end 
        end 
    end 
    unique555 = unique(SP500raw.peer5final);
    for i = 1:length(unique555)
        l = strcmp(SP500raw.peer5final,unique555(i));
        if sum(l) > 1
            for j = 2:sum(l)
                [row,col] = find(l);
                SP500raw.peer5final(row(j)) = SP500raw.peer54(row(j));
            end 
        end 
    end     
%% Section 3: Event Study for Price and Volume
% clean tickers
    % these are the tickers from SP500 deletions with available data 
    % sorted from previous SP500raw table based on Quandl availability
    control = readtable('QFE_ticker.xlsx');
    % get final data set 
    SP500 = table;
    for i = 1: height(control)
        idx = find(strcmp(SP500raw.Ticker,control.Ticker(i)));
        SP500(i,:) = SP500raw(idx,:);
    end 
    % mannually change some duplicated peers/peers without data available
    SP500.peer5final(65) = SP500.peer57(65);
    SP500.peer5final(60) = SP500.peer58(60);
    SP500.peer5final(43) = SP500.peer58(43);
    SP500.peer5final(27) = SP500.peer58(27);
    SP500.peer5final(59) = SP500.peer57(59);
    SP500.peer5final(54) = SP500.peer52(54);
    % Isolate needed variables 
    announcement = SP500.AnnouncementDate;
    y = floor(announcement/10000); % annoucement year
    m = floor(announcement/100 - y*100); % annoucement month
    d = announcement - m*100 - y*10000; % annoucement date
    announcement = datetime(y,m,d); % change annoucement to datetime for quandl pool 
    % get treatment/control tickers for paper appendix 
    removal = SP500.EffectiveThruDate;
    tickers = SP500.Ticker;
    peers1 = SP500.peer4final;
    peers2 = SP500.peer5final;
%% Enter the API key
    apikey = 'HxTGtomxL79TZzQg_Ey4'; 
% Establish database connection and pull price data
    c = quandl(apikey); 
    market = ('EOD/SPY'); % use SPY as market proxy 
% set windows
    trainwindow = calyears(1)+30; % estimate expected vol/return
    preevent_window = 30; % 30 days before annoucement
    postevent_window = 30; % 30 days after annoucement
    data_window = 70; % more days for pulling data to account for weekends 
% set dates for quandl
    start_date1 = announcement - trainwindow;  
    estimation_date = announcement + 1;
    end_date1 = announcement + data_window;   
% set dataframes 
% abnormal returns, abnormal volumn for treatment, sp400peers and sp500
% peers, respectively
    AR = zeros(preevent_window+postevent_window+1,1);
    AV = zeros(preevent_window+postevent_window+1,1);
    ARp1 = zeros(preevent_window+postevent_window+1,1);
    AVp1 = zeros(preevent_window+postevent_window+1,1);
    ARp2 = zeros(preevent_window+postevent_window+1,1);
    AVp2 = zeros(preevent_window+postevent_window+1,1);
    MV = zeros(preevent_window+postevent_window+1,1);
    % DID dataframe 
    DIDtreat = [];
    DIDw = [];
    DIDw1 = [];
    DIDw2 = [];
    DIDcontrol1 = [];
    DIDcontrol2 = [];  
%% loop over each ticker to get volumn and price dara 
    for i = 1:size(tickers,1)
        i
        s = strcat('EOD/', tickers(i)); %concatenate ticker string with rest of Quandl database identifier
        p1 = strcat('EOD/',peers1(i));% peers from 400 denoted as peer1 
        p2 = strcat('EOD/',peers2(i));% peers from 500 denoted as peer2
        % grab daily closing price and volume
        train_asset = history(c,s,start_date1(i),announcement(i), 'daily'); %pull prices for current stock
        train_ivv = history(c,market,start_date1(i),announcement(i), 'daily'); 
        train_peer1 = history(c,p1,start_date1(i),announcement(i), 'daily'); 
        train_peer2 = history(c,p2,start_date1(i),announcement(i), 'daily'); 
        est_asset = history(c,s,estimation_date(i),end_date1(i), 'daily'); 
        est_ivv = history(c,market,estimation_date(i),end_date1(i), 'daily'); 
        est_peer1 = history(c,p1,estimation_date(i),end_date1(i), 'daily');   
        est_peer2 = history(c,p2,estimation_date(i),end_date1(i), 'daily');        
        % grab pre-event price and volumn
        price = flipud(train_asset.Close); 
        mprice = flipud(train_ivv.Close); 
        p1price = flipud(train_peer1.Close);
        p2price = flipud(train_peer2.Close);
        volume = flipud(train_asset.Volume); 
        p1volume = flipud(train_peer1.Volume);
        p2volume = flipud(train_peer2.Volume);
        % grab post-event price and volumn     
        est_p = flipud(est_asset.Close); 
        est_ivvp= flipud(est_ivv.Close); 
        est_pp1 = flipud(est_peer1.Close); 
        est_pp2 = flipud(est_peer2.Close); 
        
        est_volume = flipud(est_asset.Volume); 
        est_p1volume = flipud(est_peer1.Volume);
        est_p2volume = flipud(est_peer2.Volume);
        
        % calcualte log returns
        % pre event returns
        ret = diff(log(price))*100; 
        mret = diff(log(mprice))*100;
        p1ret = diff(log(p1price))*100;
        p2ret = diff(log(p2price))*100;
        % post event returns
        est_ret = diff(log(est_p))*100;
        est_mret = diff(log(est_ivvp))*100; 
        est_p1ret = diff(log(est_pp1))*100;
        est_p2ret = diff(log(est_pp2))*100;
        % sort returns into event window
        % event window = preevent window + post evetn window 
        est_ret = [ret(end-preevent_window:end,:);est_ret(1:postevent_window,:)];
        est_mret = [mret(end-preevent_window:end,:);est_mret(1:postevent_window,:)];
        est_p1ret = [p1ret(end-preevent_window:end,:);est_p1ret(1:postevent_window,:)];
        est_p2ret = [p2ret(end-preevent_window:end,:);est_p2ret(1:postevent_window,:)];
        ret = ret(1:end-preevent_window,:);% price for estimation window 
        mret = mret(1:end-preevent_window,:);
        p1ret = p1ret(1:end-preevent_window,:);
        p2ret = p2ret(1:end-preevent_window,:);    

        % sort volume into event window
        est_v = [volume(end-preevent_window:end,:);est_volume(1:postevent_window,:)];
        est_p1v = [p1volume(end-preevent_window:end,:);est_p1volume(1:postevent_window,:)];
        est_p2v = [p2volume(end-preevent_window:end,:);est_p2volume(1:postevent_window,:)];
        v = volume(1:end-preevent_window,:);% volume for estimation window
        p1v = p1volume(1:end-preevent_window,:);
        p2v = p1volume(1:end-preevent_window,:);                
        % grab event window data for DID 
        DIDtreat = [DIDtreat est_ret];
        DIDw = [DIDw est_v];
        DIDw1 = [DIDw1 est_p1v];
        DIDw2 = [DIDw2 est_p2v];
        DIDcontrol1 = [DIDcontrol1 est_p1ret];
        DIDcontrol2 = [DIDcontrol2 est_p2ret];
        
        %run the regression with y as asset returns and x as mkt return,
        %market model
        mdl = fitlm(mret,ret);
        mdl2 = fitlm(mret,p1ret);
        mdl3 = fitlm(mret,p2ret);
        exp_ret = mdl.Coefficients.Estimate(1) + mdl.Coefficients.Estimate(2)*est_mret;
        exp_p1ret = mdl2.Coefficients.Estimate(1) + mdl2.Coefficients.Estimate(2)*est_mret;
        exp_p2ret = mdl3.Coefficients.Estimate(1) + mdl2.Coefficients.Estimate(2)*est_mret;
        % calculate abnormal returns = real return - expected return 
        AR(:,i) = est_ret - exp_ret;
        ARp1(:,i) = est_p1ret - exp_ret;
        ARp2(:,i) = est_p2ret - exp_ret;
        % compute abnormal volume, constant mean return model
        AV(:,i) = est_v - mean(v); 
        AVp1(:,i) = est_p1v - mean(p1v);
        AVp2(:,i) = est_p2v - mean(p2v);
    end  
%% convert volume to volume/total shares oustanding in percentage
    list = table([tickers;peers1;peers2]);
    writetable(list,'sharelist');
    stockshare = readtable('stockshare.xlsx');
    for i = 1:length(tickers)
        temp = stockshare(strcmp(SP500.ADQ(i),stockshare.CalendarDataYearAndQuarter),:);
        s1 = temp.CommonSharesOutstanding(strcmp(temp.TickerSymbol,tickers(i)));
        s2 = temp.CommonSharesOutstanding(strcmp(temp.TickerSymbol,peers1(i)));
        s3 = temp.CommonSharesOutstanding(strcmp(temp.TickerSymbol,peers2(i)));
        AV(:,i) = AV(:,i)/(s1*10^6)*100;
        AVp1(:,i) = AVp1(:,i)/(s2*10^6)*100;
        AVp2(:,i) = AVp2(:,i)/(s3*10^6)*100;
        DIDw(:,i) = DIDw(:,i)/(s1*10^6)*100;
        DIDw1(:,i) = DIDw1(:,i)/(s2*10^6)*100;
        DIDw2(:,i) = DIDw2(:,i)/(s3*10^6)*100;
    end
%% average AR/AV and CAR
    % CAR as sum of AR across time
    CAR = cumsum(AR);
    CARp1 = cumsum(ARp1);
    CARp2 = cumsum(ARp2);
    % average CAR across stocks
    AVCAR = mean(CAR,2);
    AVCARp1 = mean(CARp1,2);
    AVCARp2 = mean(CARp2,2);
    % average AR across stocks
    AVAR = mean(AR,2);
    AVARp1 = mean(ARp1,2);
    AVARp2 = mean(ARp2,2);
    % average AV across stocks
    AVAV = mean(AV,2);
    AVAVp1 = mean(AVp1,2);
    AVAVp2 = mean(AVp2,2);
%% significance test abnormal returns
    s = 20;
    n = 61;
    N = length(tickers);
    % stat for abnormal returns
    % deleted stock
    u1 = AVCAR(n);
    var1 = sum(var(AR(s:n,:)))/N^2;
    J1 = u1/var1^0.5;
    p1 = 1-normcdf(abs(J1));%get p-value
    % peer sp400
    u2 = AVCARp1(n);
    var2 = sum(var(ARp1(s:n,:)))/N^2;
    J2 = u2/var2^0.5;
    p2 = 1-normcdf(abs(J2));%get p-value
    % peer sp500
    u3 = AVCARp2(n);
    var3 = sum(var(ARp2(s:n,:)))/N^2;
    J3 = u3/var3^0.5;
    p3 = 1-normcdf(abs(J3));%get p-value   
    % grab confidence interval 
    CI95 = norminv([0.025 0.975]);
    u1CI95 = AVCAR + var1^0.5*CI95;
    u2CI95 = AVCARp1 + var2^0.5*CI95;
    u3CI95 = AVCARp2 + var3^0.5*CI95;
    % event window for graph
    ARdate = -preevent_window:postevent_window;
%% find the reversion point 
    p=[];
    for n = 30:61
        u = AVCAR(n);
        vari = sum(var(AR(s:n,:)))/N^2;
        J = u/vari^0.5;
        p(n-29) = 1-normcdf(abs(J));%get p-value
    end 
    % reversion of abnormal significance at n = 54-55
%% significance test abnormal volume
    s = 20;
    n = 61;
    % deleted stock
    v1 = mean(AVAV(s:n));
    vvar1 = sum(var(AV(s:n,:)))/N^2;
    vJ1 = v1/vvar1^0.5;
    vp1 = 1-normcdf(abs(vJ1));%get p-value
    % peer sp400
    v2 = mean(AVAVp1(s:n));
    vvar2 = sum(var(AVp1(s:n,:)))/N^2;
    vJ2 = v2/vvar2^0.5;
    vp2 = 1-normcdf(abs(vJ2));%get p-value
    % peer sp500
    v3 = mean(AVAVp2(s:n));
    vvar3 = sum(var(AVp2(s:n,:)))/N^2;
    vJ3 = v3/vvar3^0.5;
    vp3 = 1-normcdf(abs(vJ3));%get p-value   
    % grab confidence interval 
    v1CI95 = AVAV + vvar1^0.5*CI95;
    v2CI95 = AVAVp1 + vvar2^0.5*CI95;
    v3CI95 = AVAVp2 + vvar3^0.5*CI95;
%% graph avg CAR
    figure;
    plot(ARdate,AVCAR,'LineWidth',2)
    hold on 
    plot(ARdate,AVCARp1,'LineWidth',2)
    plot(ARdate,AVCARp2,'g','LineWidth',2)
    xlabel('Date')
    ylabel('Average Cumulative Abnormal Return Overtime')
    title('Average Cumulative Abnormal Return Overtime of Migrated Stocks vs. Peer Stocks with 95% Confidence Interval')
    % plot CI
    plot(ARdate,u1CI95,'b:')
    plot(ARdate,u2CI95,'r:')
    plot(ARdate,u3CI95,'g:')
    legend('Index Deletions','SP400peers','SP500peers','Location','best')
    hold off 
%% graph avg AV
    figure;
    plot(ARdate,AVAV,'LineWidth',2)
    hold on 
    plot(ARdate,AVAVp1,'LineWidth',2)
    plot(ARdate,AVAVp2,'g','LineWidth',2)
    xlabel('Date')
    ylabel('Average Abnormal Volume across Assets')
    title('Average Abnormal Volume across Assets of Migrated Stocks vs. Peer Stocks with 95% Confidence Interval')
    % plot CI
    plot(ARdate,v1CI95,'b:')
    plot(ARdate,v2CI95,'r:')
    plot(ARdate,v3CI95,'g:')
    legend('Index Deletions','SP400peers','SP500peers','Location','best')
    hold off 
%% Section 4: cross-section and panel regression
    % cross section regression
    s = 1;
    n = 61;
    Y = [];
    for i= 1:N
        Y = [Y;CAR(s:n,i)];
    end 
    X = [];
    for i = 1:N
        X = [X;AV(s:n,i)];
    end 
    mdl = fitlm(X,Y)
    % fixed effect regression control for individual firms 
    X6 = zeros(length(Y),67);
    for i = 1:67
        a = (n-s+1)*i;
        X6(a-(n-s):a,i) = 1;
    end 
    X = [X X6];
    mdl=fitlm(X,Y)
%% Section 5: Difference in Difference
% DID model 1
    s = 1;
    n = 51;
    % naive calculation
    dd11 = (DIDtreat(n,:) - DIDcontrol1(n,:))-(DIDtreat(s,:)-DIDcontrol1(s,:));
    dd11 = mean(dd11);
    dd12 = (DIDtreat(n,:) - DIDcontrol2(n,:))-(DIDtreat(s,:)-DIDcontrol2(s,:));
    dd12 = mean(dd12);
    % y = [pretreat;posttreat;precontrol1;postcontrol1]
    Y1 = [DIDtreat(s,:)';DIDtreat(n,:)';DIDcontrol1(s,:)';DIDcontrol1(n,:)'];
    Y2 = [DIDtreat(s,:)';DIDtreat(n,:)';DIDcontrol2(s,:)';DIDcontrol2(n,:)'];
    % set treat and post dummies
    treat = [ones(N,1);ones(N,1);zeros(N,1);zeros(N,1)];
    post = [zeros(N,1);ones(N,1);zeros(N,1);ones(N,1)];
    interaction = treat.*post;
    
    % set y = a + b1treat + b2time + b3treat*time + e
    X = [treat post interaction];
    mdl1 = fitlm(X,Y1);
    mdl2 = fitlm(X,Y2);
    % print values for paper table 
    mdl1.Coefficients.Estimate(2)
    mdl1.Coefficients.pValue(2)
    mdl2.Coefficients.Estimate(2)
    mdl2.Coefficients.pValue(2)
%% DID model 2
    % individual asset dummies
    X5 = zeros(length(Y1),length(Y1)/2);
    for i = 1:length(X5)/4
        X5(i,i) = 1;
        X5(i+length(X5)/4,i)=1;
        X5(i+length(X5)/2,i+length(X5)/4)=1;
        X5(i+length(X5)/2+length(X5)/4,i+length(X5)/4)=1;
    end 
    X5(:,1) = [];
    X5(:,1+length(X5)/4) = [];
    % time dummies
    X6 = zeros(length(Y1),length(Y1)/2);
    for i = 1:length(X6)/4
        X6(i,i)=1;
        X6(i,i+length(X6)/4)=1;
    end 
    X6(:,1)= [];
    X6(:,1+length(X6)/4) = [];
    X = [treat X5 X6];
    mdl1 = fitlm(X,Y1);
    mdl2 = fitlm(X,Y2);
    % print values for paper table 
    mdl1.Coefficients.Estimate(2)
    mdl1.Coefficients.pValue(2)
    mdl2.Coefficients.Estimate(2)
    mdl2.Coefficients.pValue(2)
